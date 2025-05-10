pub fn round_dp(number: f64, dp: i32) -> f64 {
    let power = (10_f64).powi(dp);
    (number * power).round() / power
}

pub fn gcd(a: &f64, b: &f64) -> f64 {
    let mut r = a.rem_euclid(*b);
    let mut result: f64 = b.clone();

    while r != 0.0 {
        let temp = r;
        r = result.rem_euclid(r);
        result = temp;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round() {
        assert_eq!(round_dp(1.234, 1), 1.2);
    }

    #[test]
    fn test_gcd() {
        assert_eq!(gcd(&6.0, &4.0), 2.0);
    }
}
