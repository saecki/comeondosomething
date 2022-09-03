pub(crate) fn get<T: Sized>(buf: &[u8], index: usize) -> &T {
    unsafe { std::mem::transmute(&buf[index] as *const u8) }
}

pub(crate) fn get_mut<T: Sized>(buf: &mut [u8], index: usize) -> &mut T {
    unsafe { std::mem::transmute(&buf[index] as *const u8) }
}

