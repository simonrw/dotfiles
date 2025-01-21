{
  runCommand,
  vscode-extensions,
}:
runCommand "codelldb" {
  propagatedBuildInputs = [
    vscode-extensions.vadimcn.vscode-lldb
  ];
} ''
  mkdir -p $out
  cp -r ${vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter $out/bin
''
