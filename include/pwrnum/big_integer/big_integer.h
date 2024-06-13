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

namespace pwrnum {

class BigInteger {
public:
  BigInteger() = default;

  BigInteger(const std::string& number) {
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
    RemoveLeadingZeros();
  }

  BigInteger(const BigInteger& other)
      : digits_(other.digits_), is_negative_(other.is_negative_) {}

  BigInteger(BigInteger&& other) noexcept
      : digits_(std::move(other.digits_)), is_negative_(other.is_negative_) {
    other.is_negative_ = false;
  }

  BigInteger& operator=(const BigInteger& other) {
    if (this != &other) {
      digits_ = other.digits_;
      is_negative_ = other.is_negative_;
    }
    return *this;
  }

  BigInteger& operator=(BigInteger&& other) noexcept {
    if (this != &other) {
      digits_ = std::move(other.digits_);
      is_negative_ = other.is_negative_;
      other.is_negative_ = false;
    }
    return *this;
  }

  [[nodiscard]] BigInteger Add(const BigInteger& other) const {
    if (is_negative_ == other.is_negative_) {
      BigInteger result;
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
      result.RemoveLeadingZeros();
      return result;
    }
    BigInteger abs_this = *this;
    abs_this.is_negative_ = false;
    BigInteger abs_other = other;
    abs_other.is_negative_ = false;

    if (abs_this.GreaterThan(abs_other)) {
      return abs_this.Subtract(abs_other);
    }
    BigInteger result = abs_other.Subtract(abs_this);
    result.is_negative_ = other.is_negative_;
    return result;
  }

  [[nodiscard]] BigInteger Subtract(const BigInteger& other) const {
    if (this->is_negative_ != other.is_negative_) {
      BigInteger result = Add(other);
      result.is_negative_ = this->is_negative_;
      return result;
    }
    if (Equal(other)) {
      return BigInteger{"0"};
    }
    const bool result_is_negative = LessThan(other);
    const BigInteger& larger = result_is_negative ? other : *this;
    const BigInteger& smaller = result_is_negative ? *this : other;

    BigInteger result;
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
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigInteger Multiply(const BigInteger& other) const {
    BigInteger result;
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
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigInteger Divide(const BigInteger& other) const {
    if (other.Equal(BigInteger("0"))) {
      throw std::invalid_argument("Division by zero");
    }
    BigInteger dividend = *this;
    dividend.is_negative_ = false;
    BigInteger divisor = other;
    divisor.is_negative_ = false;
    BigInteger quotient("0");
    BigInteger current("0");

    for (int i = digits_.size() - 1; i >= 0; --i) {
      current.digits_.insert(current.digits_.begin(), digits_[i]);
      current.RemoveLeadingZeros();

      int x = 0, l = 0, r = 10;
      while (l <= r) {
        const int m = (l + r) / 2;
        BigInteger t = divisor.Multiply(BigInteger(std::to_string(m)));
        if (t.LessThan(current) || t.Equal(current)) {
          x = m;
          l = m + 1;
        } else {
          r = m - 1;
        }
      }
      quotient.digits_.insert(quotient.digits_.begin(), x);
      current =
          current.Subtract(divisor.Multiply(BigInteger(std::to_string(x))));
    }

    quotient.is_negative_ = is_negative_ != other.is_negative_;
    quotient.RemoveLeadingZeros();
    return quotient;
  }

  [[nodiscard]] BigInteger Modulo(const BigInteger& other) const {
    BigInteger result = this->Subtract(this->Divide(other).Multiply(other));
    if (result.is_negative_ != this->is_negative_) {
      result.is_negative_ = this->is_negative_;
    }
    return result;
  }

  [[nodiscard]] BigInteger Abs() const {
    BigInteger result = *this;
    result.is_negative_ = false;
    return result;
  }

  [[nodiscard]] BigInteger Pow(int exponent) const {
    if (exponent < 0) {
      throw std::invalid_argument("Negative exponent not supported");
    }
    BigInteger result("1");
    BigInteger base = *this;

    while (exponent > 0) {
      if (exponent % 2 == 1) {
        result = result.Multiply(base);
      }
      base = base.Multiply(base);
      exponent /= 2;
    }
    return result;
  }

  [[nodiscard]] BigInteger Sqr() const { return this->Multiply(*this); }

  [[nodiscard]] BigInteger Sqrt() const {
    if (is_negative_) {
      throw std::invalid_argument("Square root of negative number");
    }
    if (this->Equal(BigInteger("0")) || this->Equal(BigInteger("1"))) {
      return *this;
    }

    BigInteger low("1"), high = *this, squared;
    while (low.LessThan(high) || low.Equal(high)) {
      BigInteger mid = low.Add(high).Divide(BigInteger("2"));
      squared = mid.Sqr();
      if (squared.Equal(*this)) {
        return mid;
      }
      if (squared.LessThan(*this)) {
        low = mid.Add(BigInteger("1"));
      } else {
        high = mid.Subtract(BigInteger("1"));
      }
    }
    return high;
  }

  void Swap(BigInteger& other) {
    std::swap(digits_, other.digits_);
    std::swap(is_negative_, other.is_negative_);
  }

  [[nodiscard]] bool IsPositive() const {
    return !is_negative_ && !Equal(BigInteger("0"));
  }

  [[nodiscard]] bool IsNegative() const { return is_negative_; }

  [[nodiscard]] int Compare(const BigInteger& other) const {
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

  [[nodiscard]] bool LessThan(const BigInteger& other) const {
    return Compare(other) < 0;
  }

  [[nodiscard]] bool GreaterThan(const BigInteger& other) const {
    return Compare(other) > 0;
  }

  [[nodiscard]] bool Equal(const BigInteger& other) const {
    return Compare(other) == 0;
  }

  [[nodiscard]] std::string ToString() const {
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

  void Invert() {
    if (!digits_.empty()) {
      is_negative_ = !is_negative_;
    }
  }

  [[nodiscard]] BigInteger ShiftLeft(int positions) const {
    if (positions < 0) {
      return ShiftRight(-positions);
    }
    BigInteger result = *this;
    result.digits_.insert(result.digits_.begin(), positions, 0);
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigInteger ShiftRight(int positions) const {
    if (positions < 0) {
      return ShiftLeft(-positions);
    }
    BigInteger result = *this;
    if (static_cast<size_t>(positions) >= result.digits_.size()) {
      return BigInteger{"0"};
    }
    result.digits_.erase(result.digits_.begin(),
                         result.digits_.begin() + positions);
    result.RemoveLeadingZeros();
    return result;
  }

  void RemoveLeadingZeros() {
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
