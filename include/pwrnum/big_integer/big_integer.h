// Copyright (c) 2024, Alexander Szawrowski
//
// This file is distributed under the MIT License.
// See LICENSE file for details.

#ifndef PWRNUM_BIG_INTEGER_BIG_INTEGER_H_
#define PWRNUM_BIG_INTEGER_BIG_INTEGER_H_

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace cait {

class bigint_t {
public:
  bigint_t() = default;

  bigint_t(const std::string& number) {
    if (number.empty()) {
      throw std::invalid_argument("Invalid number format");
    }
    size_t start = 0;
    if (number[0] == '-') {
      is_negative_ = true;
      start = 1;
    } else {
      is_negative_ = false;
      if (number[0] == '+') {
        start = 1;
      }
    }
    for (size_t i = start; i < number.size(); ++i) {
      if (!isdigit(number[i])) {
        throw std::invalid_argument("Invalid number format");
      }
      digits_.push_back(number[i] - '0');
    }
    std::reverse(digits_.begin(), digits_.end());
    remove_leading_zeros();
  }

  bigint_t(const bigint_t& other)
      : digits_(other.digits_), is_negative_(other.is_negative_) {}

  bigint_t(bigint_t&& other) noexcept
      : digits_(std::move(other.digits_)), is_negative_(other.is_negative_) {
    other.is_negative_ = false;
  }

  bigint_t& operator=(const bigint_t& other) {
    if (this != &other) {
      digits_ = other.digits_;
      is_negative_ = other.is_negative_;
    }
    return *this;
  }

  bigint_t& operator=(bigint_t&& other) noexcept {
    if (this != &other) {
      digits_ = std::move(other.digits_);
      is_negative_ = other.is_negative_;
      other.is_negative_ = false;
    }
    return *this;
  }

  [[nodiscard]] bigint_t add(const bigint_t& other) const {
    if (is_negative_ == other.is_negative_) {
      bigint_t result;
      result.is_negative_ = is_negative_;
      result.digits_.resize(std::max(digits_.size(), other.digits_.size()) + 1,
                            0);

      int carry = 0;
      for (size_t i = 0; i < result.digits_.size(); ++i) {
        int sum = carry;
        if (i < digits_.size()) {
          sum += digits_[i];
        }
        if (i < other.digits_.size()) {
          sum += other.digits_[i];
        }
        result.digits_[i] = sum % 10;
        carry = sum / 10;
      }
      result.remove_leading_zeros();
      return result;
    }
    bigint_t abs_this = *this;
    abs_this.is_negative_ = false;
    bigint_t abs_other = other;
    abs_other.is_negative_ = false;

    if (abs_this.greater_than(abs_other)) {
      return abs_this.subtract(abs_other);
    }
    bigint_t result = abs_other.subtract(abs_this);
    result.is_negative_ = other.is_negative_;
    return result;
  }

  [[nodiscard]] bigint_t subtract(const bigint_t& other) const {
    if (this->is_negative_ != other.is_negative_) {
      bigint_t result = add(other);
      result.is_negative_ = this->is_negative_;
      return result;
    }
    if (equal(other)) {
      return bigint_t{"0"};
    }
    const bool result_is_negative = less_than(other);
    const bigint_t& larger = result_is_negative ? other : *this;
    const bigint_t& smaller = result_is_negative ? *this : other;

    bigint_t result;
    result.is_negative_ = result_is_negative;
    result.digits_.resize(larger.digits_.size(), 0);

    int borrow = 0;
    for (size_t i = 0; i < larger.digits_.size(); ++i) {
      int diff = larger.digits_[i] - borrow;
      if (i < smaller.digits_.size()) {
        diff -= smaller.digits_[i];
      }
      if (diff < 0) {
        diff += 10;
        borrow = 1;
      } else {
        borrow = 0;
      }
      result.digits_[i] = diff;
    }
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigint_t multiply(const bigint_t& other) const {
    bigint_t result;
    result.digits_.resize(digits_.size() + other.digits_.size(), 0);

    for (size_t i = 0; i < digits_.size(); ++i) {
      int carry = 0;
      for (size_t j = 0; j < other.digits_.size() || carry != 0; ++j) {
        const int sum =
            result.digits_[i + j] +
            digits_[i] * (j < other.digits_.size() ? other.digits_[j] : 0) +
            carry;
        result.digits_[i + j] = sum % 10;
        carry = sum / 10;
      }
    }
    result.is_negative_ = is_negative_ != other.is_negative_;
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigint_t divide(const bigint_t& other) const {
    if (other.equal(bigint_t("0"))) {
      throw std::invalid_argument("Division by zero");
    }
    bigint_t dividend = *this;
    dividend.is_negative_ = false;
    bigint_t divisor = other;
    divisor.is_negative_ = false;
    bigint_t quotient("0");
    bigint_t current("0");

    for (int i = digits_.size() - 1; i >= 0; --i) {
      current.digits_.insert(current.digits_.begin(), digits_[i]);
      current.remove_leading_zeros();

      int x = 0, l = 0, r = 10;
      while (l <= r) {
        const int m = (l + r) / 2;
        bigint_t t = divisor.multiply(bigint_t(std::to_string(m)));
        if (t.less_than(current) || t.equal(current)) {
          x = m;
          l = m + 1;
        } else {
          r = m - 1;
        }
      }
      quotient.digits_.insert(quotient.digits_.begin(), x);
      current = current.subtract(divisor.multiply(bigint_t(std::to_string(x))));
    }

    quotient.is_negative_ = is_negative_ != other.is_negative_;
    quotient.remove_leading_zeros();
    return quotient;
  }

  [[nodiscard]] bigint_t modulo(const bigint_t& other) const {
    bigint_t result = this->subtract(this->divide(other).multiply(other));
    if (result.is_negative_ != this->is_negative_) {
      result.is_negative_ = this->is_negative_;
    }
    return result;
  }

  [[nodiscard]] bigint_t abs() const {
    bigint_t result = *this;
    result.is_negative_ = false;
    return result;
  }

  [[nodiscard]] bigint_t pow(int exponent) const {
    if (exponent < 0) {
      throw std::invalid_argument("Negative exponent not supported");
    }
    bigint_t result("1");
    bigint_t base = *this;

    while (exponent > 0) {
      if (exponent % 2 == 1) {
        result = result.multiply(base);
      }
      base = base.multiply(base);
      exponent /= 2;
    }
    return result;
  }

  [[nodiscard]] bigint_t sqr() const { return this->multiply(*this); }

  [[nodiscard]] bigint_t sqrt() const {
    if (is_negative_) {
      throw std::invalid_argument("Square root of negative number");
    }
    if (this->equal(bigint_t("0")) || this->equal(bigint_t("1"))) {
      return *this;
    }

    bigint_t low("1"), high = *this, squared;
    while (low.less_than(high) || low.equal(high)) {
      bigint_t mid = low.add(high).divide(bigint_t("2"));
      squared = mid.sqr();
      if (squared.equal(*this)) {
        return mid;
      }
      if (squared.less_than(*this)) {
        low = mid.add(bigint_t("1"));
      } else {
        high = mid.subtract(bigint_t("1"));
      }
    }
    return high;
  }

  void swap(bigint_t& other) {
    std::swap(digits_, other.digits_);
    std::swap(is_negative_, other.is_negative_);
  }

  [[nodiscard]] bool is_positive() const {
    return !is_negative_ && !equal(bigint_t("0"));
  }

  [[nodiscard]] bool is_negative() const { return is_negative_; }

  [[nodiscard]] int compare(const bigint_t& other) const {
    if (is_negative_ != other.is_negative_) {
      return is_negative_ ? -1 : 1;
    }

    if (digits_.size() != other.digits_.size()) {
      if (is_negative_) {
        return digits_.size() < other.digits_.size() ? 1 : -1;
      }
      return digits_.size() > other.digits_.size() ? 1 : -1;
    }

    for (int i = digits_.size() - 1; i >= 0; --i) {
      if (digits_[i] != other.digits_[i]) {
        if (is_negative_) {
          return digits_[i] < other.digits_[i] ? 1 : -1;
        }
        return digits_[i] > other.digits_[i] ? 1 : -1;
      }
    }

    return 0;
  }

  [[nodiscard]] bool less_than(const bigint_t& other) const {
    return compare(other) < 0;
  }

  [[nodiscard]] bool greater_than(const bigint_t& other) const {
    return compare(other) > 0;
  }

  [[nodiscard]] bool equal(const bigint_t& other) const {
    return compare(other) == 0;
  }

  [[nodiscard]] std::string to_string() const {
    if (digits_.empty()) {
      return "0";
    }
    std::string result;
    if (is_negative_) {
      result += "-";
    }
    for (auto it = digits_.rbegin(); it != digits_.rend(); ++it) {
      result += std::to_string(*it);
    }
    return result;
  }

  void invert() {
    if (!digits_.empty()) {
      is_negative_ = !is_negative_;
    }
  }

  [[nodiscard]] bigint_t shift_left(int positions) const {
    if (positions < 0) {
      return shift_right(-positions);
    }
    bigint_t result = *this;
    result.digits_.insert(result.digits_.begin(), positions, 0);
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigint_t shift_right(int positions) const {
    if (positions < 0) {
      return shift_left(-positions);
    }
    bigint_t result = *this;
    if (static_cast<size_t>(positions) >= result.digits_.size()) {
      return bigint_t{"0"};
    }
    result.digits_.erase(result.digits_.begin(),
                         result.digits_.begin() + positions);
    result.remove_leading_zeros();
    return result;
  }

  void remove_leading_zeros() {
    while (!digits_.empty() && digits_.back() == 0) {
      digits_.pop_back();
    }
    if (digits_.empty()) {
      is_negative_ = false;
    }
  }

private:
  std::vector<int> digits_;
  bool is_negative_{};
};

}  // namespace pwrnum

#endif  // PWRNUM_BIG_INTEGER_BIG_INTEGER_H_
