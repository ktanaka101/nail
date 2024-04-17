const { defineConfig } = require("@vscode/test-cli");

module.exports = defineConfig([
  {
    label: "unitTests",
    files: "out/test/**/*.test.js",
    version: "1.88.0",
    workspaceFolder: "./sampleWorkspace",
    mocha: {
      ui: "tdd",
      timeout: 20000,
    },
  },
  // you can specify additional test configurations, too
]);
