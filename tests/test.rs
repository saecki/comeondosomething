#[test]
fn test1() {
    assert_eq!(
        20713257.3385426,
        comeondosomething::calc("234.4234 + 6345.423 * 3264.2462").unwrap(),
    );
}

#[test]
fn test2() {
    assert_eq!(
        -9717750.0,
        comeondosomething::calc("6 + 3452 − 3252 × 5324 + 2342 × 3242 ÷ 4234 × 4234 − 324")
            .unwrap(),
    );
}

#[test]
fn test3() {
    assert_eq!(
        41968480425.587155963,
        comeondosomething::calc("(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234").unwrap(),
    );
}
