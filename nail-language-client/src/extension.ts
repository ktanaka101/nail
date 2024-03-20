import * as vscode from "vscode";
import {
  type Executable,
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: vscode.ExtensionContext) {
  console.log('"nail-language-client" is now initialize!');

  const command =
    process.env.NAIL_LANGUAGE_SERVER_PATH || "nail-language-server";
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  const traceOutputChannel = vscode.window.createOutputChannel(
    "Nail Language Server trace",
  );
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "nail" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
    },
    traceOutputChannel,
  };

  client = new LanguageClient(
    "nail-language-server",
    "nail language server",
    serverOptions,
    clientOptions,
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
