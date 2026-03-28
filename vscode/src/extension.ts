import { arch, platform } from "os";
import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

const extensionName = "tablegen-lsp";
const extensionDisplayName = "TableGen Language Server";
const extensionId = `arata-nvm.${extensionName}`;
const languageId = "tablegen";

const commandRestartServer = "tablegen-lsp.restartServer";
const commandSetSourceRoot = "tablegen-lsp.setSourceRoot";
const commandClearSourceRoot = "tablegen-lsp.clearSourceRoot";
const commandOpenSourceRoot = "tablegen-lsp.openSourceRoot";

const lspNotificationSetSourceRoot = "tablegenLsp/setSourceRoot";
const lspNotificationClearSourceRoot = "tablegenLsp/clearSourceRoot";

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel | undefined;
let sourceRoot: vscode.Uri | null = null;

export function activate(context: vscode.ExtensionContext) {
    initialize(context).catch((err) => {
        vscode.window.showErrorMessage(`Error initializing tablegen-lsp: ${err}`);
        throw err;
    });
}

async function initialize(context: vscode.ExtensionContext): Promise<void> {
    outputChannel = vscode.window.createOutputChannel(extensionDisplayName);
    const statusBarItem = initStatusBarItem();

    context.subscriptions.push(
        outputChannel,
        vscode.commands.registerCommand(commandRestartServer, async () => {
            await client?.dispose();
            client = createClient(context);
            await client.start();
        }),
        vscode.commands.registerCommand(commandSetSourceRoot, async (uri: vscode.Uri | undefined) => {
            const document = vscode.window.activeTextEditor?.document;
            if (uri) {
                sourceRoot = uri;
            } else if (document?.uri.fsPath.endsWith(".td")) {
                sourceRoot = document.uri;
            } else {
                vscode.window.showInformationMessage("No file selected.");
                return;
            }

            updateStatusBarItem(statusBarItem);
            await client?.sendNotification(lspNotificationSetSourceRoot, { uri: sourceRoot.toString() });
        }),
        vscode.commands.registerCommand(commandClearSourceRoot, async () => {
            sourceRoot = null;
            updateStatusBarItem(statusBarItem);
            await client?.sendNotification(lspNotificationClearSourceRoot, {});
        }),
        vscode.commands.registerCommand(commandOpenSourceRoot, async () => {
            if (!sourceRoot) {
                vscode.window.showInformationMessage("No source root set.");
                return;
            }

            const document = await vscode.workspace.openTextDocument(sourceRoot);
            await vscode.window.showTextDocument(document);
        }),
        vscode.workspace.onDidChangeConfiguration(async (event) => {
            if (!event.affectsConfiguration(extensionName)) {
                return;
            }

            const message =
                "The tablegen-lsp configuration has been updated. Please restart the language server for the changes to take effect.";
            const userResponse = await vscode.window.showInformationMessage(message, "Restart now");
            if (userResponse) {
                await vscode.commands.executeCommand(commandRestartServer);
            }
        }),
        statusBarItem
    );

    client = createClient(context);
    await client.start();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}

function createClient(context: vscode.ExtensionContext): LanguageClient {
    const serverOptions: ServerOptions = {
        run: {
            command: getServer(),
        },
        debug: {
            command: context.asAbsolutePath(path.join("..", "target", "debug", "lsp")),
            options: {
                env: Object.assign({}, process.env, {
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    RUST_BACKTRACE: "1",
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    RUST_LOG: "DEBUG",
                }),
            },
        },
    };

    const config = vscode.workspace.getConfiguration(extensionName);
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: languageId }],
        initializationOptions: config,
        outputChannel,
    };

    return new LanguageClient(
        extensionName,
        extensionDisplayName,
        serverOptions,
        clientOptions,
    );
}

function getServer(): string {
    const serverDir = path.join(__dirname, "..", "server");
    const extension = vscode.extensions.getExtension(extensionId)?.packageJSON as { version: string } | undefined;
    const version = extension?.version;
    switch (platform()) {
        case "darwin":
            switch (arch()) {
                case "arm64":
                    return path.join(serverDir, `tablegen-lsp-${version}-aarch64-apple-darwin`);
            }
            break;
        case "linux":
            switch (arch()) {
                case "arm64":
                    return path.join(serverDir, `tablegen-lsp-${version}-aarch64-unknown-linux-gnu`);
                case "x64":
                    return path.join(serverDir, `tablegen-lsp-${version}-x86_64-unknown-linux-gnu`);
            }
            break;
        case "win32":
            switch (arch()) {
                case "arm64":
                    return path.join(serverDir, `tablegen-lsp-${version}-aarch64-pc-windows-msvc.exe`);
                case "x64":
                    return path.join(serverDir, `tablegen-lsp-${version}-x86_64-pc-windows-msvc.exe`);
            }
            break;
    }

    throw new Error("This platform is currently not supported");
}

function initStatusBarItem(): vscode.StatusBarItem {
    const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 10);
    statusBarItem.command = commandOpenSourceRoot;
    statusBarItem.tooltip = "Go to TableGen source root";
    statusBarItem.show();

    const sourceRootPath = vscode.workspace.getConfiguration("tablegen-lsp").get("defaultSourceRootPath");
    if (sourceRootPath && typeof sourceRootPath === "string") {
        sourceRoot = vscode.Uri.file(sourceRootPath);
    }
    updateStatusBarItem(statusBarItem);

    return statusBarItem;
}

function updateStatusBarItem(statusBarItem: vscode.StatusBarItem) {
    if (sourceRoot) {
        const sourceRootFileName = sourceRoot.fsPath.split(path.sep).pop();
        statusBarItem.text = `$(root-folder) ${sourceRootFileName}`;
    } else {
        statusBarItem.text = "$(root-folder) No Source Root";
    }
}
