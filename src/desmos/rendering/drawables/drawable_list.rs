use crate::desmos::rendering::drawables::Drawable;

#[derive(Default)]
pub struct DrawableList<'a> {
    drawables: Vec<Drawable<'a>>,
    indices: Vec<usize>,
}

impl<'a> DrawableList<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, index: usize, drawable: Drawable<'a>) {
        let draw_index = index;
        let index = self
            .indices
            .iter()
            .rposition(|&v| v <= draw_index)
            .unwrap_or(self.indices.len());
        self.indices.insert(index, draw_index);
        self.drawables.insert(index, drawable);
    }
}

impl<'a> From<DrawableList<'a>> for Vec<Drawable<'a>> {
    fn from(value: DrawableList<'a>) -> Self {
        let mut x = value.drawables;
        x.reverse();
        x
    }
}
