use assert_cmd::Command;

#[test]
fn dyn_capture() {
    let input = "\
fun outer() {
    val a = 3

    fun inner() -> int {
        a
    }
}
";
    let output = "\
[1;94m2 │[0m     val a = 3
  [1;94m│[0m         [1;91m^[0m
[1;94m3 │[0m 
[1;94m4 │[0m     fun inner() -> int {
[1;94m5 │[0m         a
  [1;94m│[0m         [1;91m^[0m
  [1;94m│[0m [1;91mCapturing variables from a dynamic scope is not yet implemented[0m[0m

";

    Command::cargo_bin("cods")
        .unwrap()
        .arg(input)
        .assert()
        .failure()
        .stdout(output);
}

#[test]
fn mark_error_after_line_end() {
    let input = "4 -";
    let output = "\
[1;94m1 │[0m 4 -
  [1;94m│[0m    [1;91m^[0m
  [1;94m│[0m [1;91mMissing operand[0m[0m

";

    Command::cargo_bin("cods")
        .unwrap()
        .arg(input)
        .assert()
        .failure()
        .stdout(output);
}
