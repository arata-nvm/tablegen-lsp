import { arch, platform } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
	initialize(context).catch((err) => {
		vscode.window.showErrorMessage(`Error initializing tablegen-lsp: ${err}`);
		throw err;
	});
}

async function initialize(context: vscode.ExtensionContext): Promise<void> {
	const serverOptions: ServerOptions = {
		run: {
			command: getServer(),
		},
		debug: {
			command: context.asAbsolutePath(path.join('..', 'target', 'debug', 'tablegen-lsp')),
			options: { env: Object.assign({}, process.env, { RUST_BACKTRACE: '1' }), }
		},
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'tablegen' }],
	};

	client = new LanguageClient('tablegen-lsp', 'TableGen Language Server', serverOptions, clientOptions);

	context.subscriptions.push(
		vscode.commands.registerCommand('tablegen-lsp.restartServer', () => client.restart())
	);

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	return client?.stop();
}

function getServer(): string {
	switch (platform()) {
		case 'darwin':
			switch (arch()) {
				case 'arm64':
					return 'tablegen-lsp-master-aarch64-apple-darwin';
				case 'x64':
					return 'tablegen-lsp-master-x86_64-apple-darwin';
			}
			break;
		case 'linux':
			switch (arch()) {
				case 'arm64':
					return 'tablegen-lsp-master-aarch64-unknown-linux-gnu';
				case 'x64':
					return 'tablegen-lsp-master-x86_64-unknown-linux-gnu';
			}
			break;
		case 'win32':
			return 'tablegen-lsp-master-x86_64-pc-windows-msvc.exe';
	}

	throw new Error('This platform is currently not supported');
}
