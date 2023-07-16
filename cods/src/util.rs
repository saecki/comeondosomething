pub(crate) fn get<'a, T: Sized>(buf: &'a [u8], index: usize) -> &'a T {
    unsafe { std::mem::transmute(&buf[index] as *const u8) }
}

pub(crate) fn mut_ref<'a, T: Sized>(buf: &'a mut [u8], index: usize) -> &'a mut T {
    unsafe { std::mem::transmute(&buf[index] as *const u8) }
}

