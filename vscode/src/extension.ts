import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
	const command = context.asAbsolutePath(
		path.join('..', 'target', 'debug', 'tablegen-lsp')
	);
	const serverOptions: ServerOptions = { command };

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'tablegen' }],
	};

	client = new LanguageClient('tablegen-lsp', serverOptions, clientOptions);
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
