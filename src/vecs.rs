pub fn vec_concat<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}

pub fn iter_concat<T>(mut a: Vec<T>, b: impl IntoIterator<Item = T>) -> Vec<T> {
    a.extend(b);
    a
}
