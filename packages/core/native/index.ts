import { Elm } from "../src/Main";
import * as fs from "fs";
import * as glob from "glob";
import * as path from "path";
import chalk from "chalk";
import chokidar from "chokidar";

export type Options = {
  projectPath: string;
  watch: boolean;
  outputDir?: string;
};

const watchFiles = ["Index.elm", "Main.elm", "Pages/**/*.elm"];

export function buildApp({ projectPath, watch, outputDir }: Options) {
  const program = Elm.Main.init({ flags: null });

  const root = path.join(projectPath, "src");
  const distRoot = outputDir || path.join(root, "App");
  const distRelative = path.relative(root, distRoot);

  // program.ports.requestSourceCode.subscribe(async (filePath) => {
  //   const contents = fs.readFileSync(path.join(root, filePath)).toString();

  //   program.ports.readSourceCode.send([filePath, contents]);
  // });

  program.ports.writeResult.subscribe(async (result) => {
    console.log(`${chalk.underline.bgGreen("Writing files:")}`);

    result.forEach(([filePath, content]) => {
      const fullPath = `${distRoot}/${filePath}`;

      ensureDirExists(fullPath);
      fs.writeFileSync(fullPath, content);

      console.log(`${filePath}`);
    });
  });

  program.ports.printError.subscribe(async (error) => {
    console.log(error);
  });

  if (watch) {
    console.log(`${chalk.bgGreenBright("watching...")}`);

    chokidar
      .watch(watchFiles, {
        ignoreInitial: true,
        cwd: root,
      })
      .on("all", (filePath) => {
        console.log(`File ${filePath} changed.`);
        // getSourcePaths();
        readSourceCode(filePath);
      });
  }

  // getSourcePaths();

  // function getSourcePaths() {
  //   const pattern = `{${watchFiles.map((x) => "/" + x).join(",")}}`;

  //   const files = glob
  //     .sync(pattern, {
  //       sync: true,
  //       ignore: [`/${distRelative}/**`],
  //       root,
  //     })
  //     .map((x) => path.relative(root, x));

  //   console.log(`${chalk.underline.bgBlue("Read files:")}`);
  //   console.log(`${files.join("\n")}`);

  //   program.ports.readSourcePaths.send(files);
  // }

  getSourceCode();

  function getSourceCode() {
    const pattern = `{${watchFiles.map((x) => "/" + x).join(",")}}`;

    const files = glob
      .sync(pattern, {
        sync: true,
        ignore: [`/${distRelative}/**`],
        root,
      })
      .map((x) => path.relative(root, x));

    console.log(`${chalk.underline.bgBlue("Read files:")}`);
    console.log(`${files.join("\n")}`);

    readSourceCode(files[0]);
  }

  function readSourceCode(filePath: string) {
    const contents = fs.readFileSync(path.join(root, filePath)).toString();
    program.ports.readSourceCode.send([filePath, contents]);
  }
}

function ensureDirExists(filePath: string) {
  return fs.mkdirSync(path.dirname(filePath), {
    recursive: true,
  });
}
