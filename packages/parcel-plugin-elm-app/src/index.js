const { buildApp } = require("elmercy");

buildApp({ projectPath: ".", watch: true });

module.exports = function(bundler) {
  bundler.on("buildStart", () => {});
};
