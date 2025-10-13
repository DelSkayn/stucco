import { basicSetup, surroundExtractor } from "testit";

export default basicSetup({
  extension: "stucco",
  prebuild: ["cargo", "build", "--example", "type_check"],
  command: ["../target/type_check"],
  path: "type_check",
  config: {
    extractor: surroundExtractor("/**", "**/"),
    type: "toml",
  }
})
