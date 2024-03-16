import { workspace, ExtensionContext, window } from 'vscode';

import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  console.log('"nail-language-client" is now initialize!');

  const command =
    process.env.NAIL_LANGUAGE_SERVER_PATH || 'nail-language-server';
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: 'debug',
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  const traceOutputChannel = window.createOutputChannel(
    'Nail Language Server trace'
  );
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'nail' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
    },
    traceOutputChannel,
  };

  client = new LanguageClient(
    'nail-language-server',
    'nail language server',
    serverOptions,
    clientOptions
  );
  await client.start();

  console.log('"nail-language-client" is now started!');
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
