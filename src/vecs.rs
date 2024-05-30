pub fn vec_concat<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}
pub fn iter_concat<T>(mut a: Vec<T>, b: impl IntoIterator<Item = T>) -> Vec<T> {
    a.extend(b);
    a
}
pub fn opt_vec_concat<T>(mut a: Option<Vec<T>>, mut b: Option<Vec<T>>) -> Option<Vec<T>> {
    match (a, b) {
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        (Some(x), Some(y)) => Some(vec_concat(x, y)),
        (None, None) => None,
    }
}
