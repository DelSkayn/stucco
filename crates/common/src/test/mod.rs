use std::{
    panic::{AssertUnwindSafe, Location},
    path::{Path, PathBuf},
};

static SEPERATOR: &str = "/************";
static CLOSER: &str = "*/";

trait StrExt {
    fn trim_suffixx<'a>(&'a self, s: &str) -> &'a str;
}

impl StrExt for str {
    fn trim_suffixx<'a>(&'a self, s: &str) -> &'a str {
        if let Some(x) = self.strip_suffix(s) {
            x
        } else {
            self
        }
    }
}

#[track_caller]
pub fn current_file_path() -> PathBuf {
    let manifest_path = env!("CARGO_MANIFEST_DIR")
        .strip_suffix("crates/common")
        .unwrap();
    let file = Location::caller().file();
    Path::new(manifest_path)
        .join(file)
        .parent()
        .unwrap()
        .canonicalize()
        .unwrap()
}

enum TestResult {
    Default,
    Accept,
    Overwrite,
}

struct TestCase {
    case: String,
    expected: Option<String>,
    path: PathBuf,
}

pub fn string_test_runner<F: Fn(&str) -> String>(path: &Path, f: F) {
    let mut tests = Vec::new();
    walk_test_dir(path, &mut tests);

    let results = match std::env::var("RESULTS").as_deref() {
        Ok("overwrite") => TestResult::Overwrite,
        Ok("accept") => TestResult::Accept,
        Err(_) | Ok(_) => TestResult::Default,
    };

    let mut successfull = true;

    for case in tests {
        let res = match std::panic::catch_unwind(AssertUnwindSafe(|| f(&case.case))) {
            Err(e) => {
                println!("Test `{}` panicked", case.path.display());
                if let Some(x) = e.downcast_ref::<&'static str>() {
                    println!("{x}");
                }
                if let Some(x) = e.downcast_ref::<String>() {
                    println!("{x}");
                }
                continue;
            }
            Ok(x) => x,
        };
        let res = res.trim();

        if let Some(expect) = case.expected.as_ref() {
            if *expect == res {
                continue;
            }
            successfull = false;
            println!("Test `{}` failed", case.path.display());
            println!("# Expected:");
            println!("{}", expect);
            println!("# Got:");
            println!("{}", res);
            println!("# Diff:");
            for c in dissimilar::diff(expect, res) {
                match c {
                    dissimilar::Chunk::Equal(s) => print!("{s}"),
                    dissimilar::Chunk::Delete(s) => print!("\x1b[31m{s}\x1b[m"),
                    dissimilar::Chunk::Insert(s) => print!("\x1b[32m{s}\x1b[m"),
                }
            }

            if let TestResult::Overwrite = results {
                std::fs::write(
                    case.path,
                    format!(
                        "{}\n{}\n{}\n{}\n",
                        case.case.trim(),
                        SEPERATOR,
                        res.trim(),
                        CLOSER,
                    ),
                )
                .unwrap();
            }
        } else {
            successfull = false;
            println!("Test `{}` had no results", case.path.display());
            println!("# Got:");
            println!("{}", res);
            if matches!(results, TestResult::Accept | TestResult::Overwrite) {
                std::fs::write(
                    case.path,
                    format!(
                        "{}\n{}\n{}\n{}\n",
                        case.case.trim(),
                        SEPERATOR,
                        res.trim(),
                        CLOSER
                    ),
                )
                .unwrap();
            }
        }

        println!();
        println!();
        println!();
    }

    if !successfull {
        panic!("Not all tests successfull")
    }
}

fn walk_test_dir(dir: &Path, tests: &mut Vec<TestCase>) {
    for ent in std::fs::read_dir(dir).unwrap() {
        let ent = ent.unwrap();
        let meta = ent.metadata().unwrap();
        let path = ent.path();
        if meta.is_file() {
            if path.extension().and_then(|x| x.to_str()) == Some("txt") {
                let source = std::fs::read_to_string(ent.path()).unwrap();

                if let Some(sep) = source.find(SEPERATOR) {
                    tests.push(TestCase {
                        path,
                        case: source[..sep].trim().to_string(),
                        expected: Some(
                            source[(sep + SEPERATOR.len())..]
                                .trim()
                                .trim_suffixx(CLOSER)
                                .trim_end()
                                .to_string(),
                        ),
                    })
                } else {
                    tests.push(TestCase {
                        path,
                        case: source.trim().to_string(),
                        expected: None,
                    })
                }
            }
        } else if meta.is_dir() {
            walk_test_dir(&path, tests);
        }
    }
}

#[test]
fn test_current_file_dir() {
    assert!(
        current_file_path()
            .display()
            .to_string()
            .ends_with("/crates/common/src/test")
    );
}

#[test]
fn string_test_test() {
    string_test_runner(&current_file_path().join("string_tests"), |x| {
        (x.parse::<i64>().unwrap() + 1).to_string()
    });
}

#[test]
#[should_panic = "Not all tests successfull"]
fn string_test_test_no_result() {
    let path = current_file_path().join("string_tests_no_result");
    // overwrite the test so that it isn't accidentally updated with TEST_RESULTS
    std::fs::write(path.join("test.txt"), "1").unwrap();

    string_test_runner(&path, |x| (x.parse::<i64>().unwrap() + 1).to_string());
}
