import { arch, platform } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
	var binary = '';
	switch (platform()) {
		case 'darwin':
			switch (arch()) {
				case 'arm64':
					binary = 'tablegen-lsp-master-aarch64-apple-darwin';
					break;
				case 'x64':
					binary = 'tablegen-lsp-master-x86_64-apple-darwin';
					break;
			}
			break;
		case 'linux':
			switch (arch()) {
				case 'arm64':
					binary = 'tablegen-lsp-master-aarch64-unknown-linux-gnu';
					break;
				case 'x64':
					binary = 'tablegen-lsp-master-x86_64-unknown-linux-gnu';
					break;
			}
			break;
		case 'win32':
			binary = 'tablegen-lsp-master-x86_64-pc-windows-msvc.exe';
			break;
	}

	if (binary === '') {
		vscode.window.showInformationMessage('This platform is currently not supported');
		return;
	}

	const command = context.asAbsolutePath(
		path.join('binary', binary)
	);
	const serverOptions: ServerOptions = { command };

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'tablegen' }],
	};

	client = new LanguageClient('tablegen-lsp', serverOptions, clientOptions);
	client.start();

	vscode.commands.registerCommand('tablegen-lsp.restartServer', () => client.restart());
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
