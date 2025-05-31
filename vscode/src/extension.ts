import { arch, platform } from "os";
import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

const extensionId = "tablegen-lsp";
const languageId = "tablegen";

const commandRestartServer = "tablegen-lsp.restartServer";
const commandSetSourceRoot = "tablegen-lsp.setSourceRoot";
const commandClearSourceRoot = "tablegen-lsp.clearSourceRoot";
const commandOpenSourceRoot = "tablegen-lsp.openSourceRoot";

let client: LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem | undefined;
let sourceRoot: vscode.Uri | null = null;

export function activate(context: vscode.ExtensionContext) {
    initialize(context).catch((err) => {
        vscode.window.showErrorMessage(`Error initializing tablegen-lsp: ${err}`);
        throw err;
    });
}

async function initialize(context: vscode.ExtensionContext): Promise<void> {
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 1000);
    statusBarItem.command = commandOpenSourceRoot;
    statusBarItem.tooltip = "Go to TableGen source root";
    updateStatusBarItem();
    statusBarItem.show();

    context.subscriptions.push(
        vscode.commands.registerCommand(commandRestartServer, async () => {
            // FIXME: OUTPUTのtablegen-lspの項目が増殖する
            await client?.dispose();
            client = createClient(context);
            await client.start();
        }),
        vscode.commands.registerCommand(commandSetSourceRoot, (uri: vscode.Uri | undefined) => {
            const document = vscode.window.activeTextEditor?.document;
            if (uri) {
                sourceRoot = uri;
            } else if (document?.uri.fsPath.endsWith(".td")) {
                sourceRoot = document.uri;
            } else {
                vscode.window.showInformationMessage("No file selected.");
                return;
            }

            updateStatusBarItem();
        }),
        vscode.commands.registerCommand(commandClearSourceRoot, () => {
            sourceRoot = null;
            updateStatusBarItem();
        }),
        vscode.commands.registerCommand(commandOpenSourceRoot, async () => {
            if (!sourceRoot) {
                vscode.window.showInformationMessage("No source root set.");
                return;
            }

            const document = await vscode.workspace.openTextDocument(sourceRoot);
            await vscode.window.showTextDocument(document);
        }),
        statusBarItem
    );

    vscode.workspace.onDidChangeConfiguration(async (event) => {
        if (!event.affectsConfiguration(extensionId)) {
            return;
        }

        const message =
            "The tablegen-lsp configuration has been updated. Please restart the language server for the changes to take effect.";
        const userResponse = await vscode.window.showInformationMessage(message, "Restart now");
        if (userResponse) {
            await vscode.commands.executeCommand(commandRestartServer);
        }
    });

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

    const config = vscode.workspace.getConfiguration(extensionId);
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: languageId }],
        initializationOptions: config,
    };

    return new LanguageClient(
        extensionId,
        "TableGen Language Server",
        serverOptions,
        clientOptions,
    );
}

function getServer(): string {
    switch (platform()) {
        case "darwin":
            switch (arch()) {
                case "arm64":
                    return "tablegen-lsp-master-aarch64-apple-darwin";
                case "x64":
                    return "tablegen-lsp-master-x86_64-apple-darwin";
            }
            break;
        case "linux":
            switch (arch()) {
                case "arm64":
                    return "tablegen-lsp-master-aarch64-unknown-linux-gnu";
                case "x64":
                    return "tablegen-lsp-master-x86_64-unknown-linux-gnu";
            }
            break;
        case "win32":
            return "tablegen-lsp-master-x86_64-pc-windows-msvc.exe";
    }

    throw new Error("This platform is currently not supported");
}

function updateStatusBarItem() {
    if (statusBarItem) {
        if (sourceRoot) {
            const sourceRootFileName = sourceRoot.fsPath.split(path.sep).pop();
            statusBarItem.text = `$(root-folder) ${sourceRootFileName}`;
        } else {
            statusBarItem.text = "$(root-folder) No Source Root";
        }
    }
}