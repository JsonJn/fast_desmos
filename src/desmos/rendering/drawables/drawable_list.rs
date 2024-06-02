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

    pub fn insert(&mut self, draw_index: usize, drawable: Drawable<'a>) {
        let index = self
            .indices
            .iter()
            .position(|&v| v >= draw_index)
            .unwrap_or(self.drawables.len());
        self.indices.insert(index, draw_index);
        self.drawables.insert(index, drawable);
    }
}

impl<'a> From<DrawableList<'a>> for Vec<Drawable<'a>> {
    fn from(value: DrawableList<'a>) -> Self {
        value.drawables
    }
}
