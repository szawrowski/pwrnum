// Copyright (c) 2024, Alexander Szawrowski
//
// This file is distributed under the MIT License.
// See LICENSE file for details.

#ifndef PWRNUM_BIG_FLOAT_BIG_FLOAT_H_
#define PWRNUM_BIG_FLOAT_BIG_FLOAT_H_

#include <algorithm>
#include <cmath>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "pwrnum/big_integer/big_integer.h"

namespace pwrnum {

class BigFloat {
public:
  BigFloat() : exponent_("0"), is_negative_(false) {}

  BigFloat(const std::string& number) {
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

    const size_t dot_pos = number.find('.');
    if (dot_pos != std::string::npos) {
      const std::string int_part = number.substr(start, dot_pos - start);
      const std::string frac_part = number.substr(dot_pos + 1);

      if (int_part.empty() && frac_part.empty()) {
        throw std::invalid_argument("Invalid number format");
      }

      if (!int_part.empty()) {
        mantissa_ = BigInteger(int_part);
      } else {
        mantissa_ = BigInteger("0");
      }
      if (!frac_part.empty()) {
        exponent_ =
            BigInteger(std::to_string(-static_cast<int>(frac_part.size())));
        mantissa_ =
            mantissa_
                .Multiply(BigInteger("1" + std::string(frac_part.size(), '0')))
                .Add(BigInteger(frac_part));
      } else {
        exponent_ = BigInteger("0");
      }
    } else {
      mantissa_ = BigInteger(number.substr(start));
      exponent_ = BigInteger("0");
    }
    RemoveLeadingZeros();
  }

  BigFloat(const BigFloat& other)
      : mantissa_(other.mantissa_),
        exponent_(other.exponent_),
        is_negative_(other.is_negative_) {}

  BigFloat(BigFloat&& other) noexcept
      : mantissa_(std::move(other.mantissa_)),
        exponent_(std::move(other.exponent_)),
        is_negative_(other.is_negative_) {
    other.is_negative_ = false;
  }

  BigFloat& operator=(const BigFloat& other) {
    if (this != &other) {
      mantissa_ = other.mantissa_;
      exponent_ = other.exponent_;
      is_negative_ = other.is_negative_;
    }
    return *this;
  }

  BigFloat& operator=(BigFloat&& other) noexcept {
    if (this != &other) {
      mantissa_ = std::move(other.mantissa_);
      exponent_ = std::move(other.exponent_);
      is_negative_ = other.is_negative_;
      other.is_negative_ = false;
    }
    return *this;
  }

  [[nodiscard]] BigFloat Add(const BigFloat& other) const {
    BigFloat lhs = *this;
    BigFloat rhs = other;

    Normalize(lhs, rhs);

    BigFloat result;
    result.mantissa_ = lhs.mantissa_.Add(rhs.mantissa_);
    result.exponent_ = lhs.exponent_;
    result.is_negative_ = lhs.is_negative_;
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigFloat Subtract(const BigFloat& other) const {
    BigFloat lhs = *this;
    BigFloat rhs = other;

    Normalize(lhs, rhs);

    BigFloat result;
    result.mantissa_ = lhs.mantissa_.Subtract(rhs.mantissa_);
    result.exponent_ = lhs.exponent_;
    result.is_negative_ = lhs.is_negative_;
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigFloat Multiply(const BigFloat& other) const {
    BigFloat result;
    result.mantissa_ = mantissa_.Multiply(other.mantissa_);
    result.exponent_ = exponent_.Add(other.exponent_);
    result.is_negative_ = is_negative_ != other.is_negative_;
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] BigFloat Divide(const BigFloat& other) const {
    if (other.Equal(BigFloat("0"))) {
      throw std::invalid_argument("Division by zero");
    }

    BigFloat result;
    result.mantissa_ = mantissa_.Divide(other.mantissa_);
    result.exponent_ = exponent_.Subtract(other.exponent_);
    result.is_negative_ = is_negative_ != other.is_negative_;
    result.RemoveLeadingZeros();
    return result;
  }

  [[nodiscard]] int Compare(const BigFloat& other) const {
    BigFloat lhs = *this;
    BigFloat rhs = other;

    Normalize(lhs, rhs);

    return lhs.mantissa_.Compare(rhs.mantissa_);
  }

  [[nodiscard]] bool LessThan(const BigFloat& other) const {
    return Compare(other) < 0;
  }

  [[nodiscard]] bool GreaterThan(const BigFloat& other) const {
    return Compare(other) > 0;
  }

  [[nodiscard]] bool Equal(const BigFloat& other) const {
    return Compare(other) == 0;
  }

  [[nodiscard]] BigFloat Abs() const {
    BigFloat result = *this;
    result.is_negative_ = false;
    return result;
  }

  [[nodiscard]] BigFloat Pow(int exponent) const {
    if (exponent < 0) {
      throw std::invalid_argument("Negative exponent not supported");
    }

    BigFloat result("1");
    BigFloat base = *this;

    while (exponent > 0) {
      if (exponent % 2 == 1) {
        result = result.Multiply(base);
      }
      base = base.Multiply(base);
      exponent /= 2;
    }

    return result;
  }

  [[nodiscard]] BigFloat Sqr() const { return this->Multiply(*this); }

  [[nodiscard]] BigFloat Sqrt() const {
    if (is_negative_) {
      throw std::invalid_argument("Square root of negative number");
    }

    if (this->Equal(BigFloat("0")) || this->Equal(BigFloat("1"))) {
      return *this;
    }

    BigFloat low("1"), high = *this;
    while (low.LessThan(high) || low.Equal(high)) {
      auto mid = low.Add(high).Divide(BigFloat("2"));
      auto squared = mid.Sqr();
      if (squared.Equal(*this)) {
        return mid;
      }
      if (squared.LessThan(*this)) {
        low = mid.Add(BigFloat("1"));
      } else {
        high = mid.Subtract(BigFloat("1"));
      }
    }

    return high;
  }

  void Swap(BigFloat& other) {
    std::swap(mantissa_, other.mantissa_);
    std::swap(exponent_, other.exponent_);
    std::swap(is_negative_, other.is_negative_);
  }

  [[nodiscard]] bool IsPositive() const {
    return !is_negative_ && !Equal(BigFloat("0"));
  }

  [[nodiscard]] bool IsNegative() const { return is_negative_; }

  [[nodiscard]] std::string ToString() const {
    std::ostringstream oss;
    if (is_negative_ && !mantissa_.Equal(BigInteger("0"))) {
      oss << "-";
    }

    const std::string mantissa_str = mantissa_.ToString();
    const int exp_val = std::stoi(exponent_.ToString());
    if (exp_val < 0) {
      const size_t point_pos = mantissa_str.size() + exp_val;
      if (point_pos <= 0) {
        oss << "0.";
        for (size_t i = 0; i < -point_pos; ++i) {
          oss << "0";
        }
        oss << mantissa_str;
      } else if (point_pos >= 0 && point_pos <= mantissa_str.size()) {
        oss << mantissa_str.substr(0, point_pos) << "."
            << mantissa_str.substr(point_pos, mantissa_str.size() - point_pos);
      } else {
        // If point_pos > mantissa_str.size()
        oss << "0";
      }
    } else {
      oss << mantissa_str;
      for (int i = 0; i < exp_val; ++i) {
        oss << "0";
      }
    }

    return oss.str();
  }

private:
  void RemoveLeadingZeros() {
    mantissa_.RemoveLeadingZeros();
    if (mantissa_.Equal(BigInteger("0"))) {
      exponent_ = BigInteger("0");
      is_negative_ = false;
    }
  }

  void Normalize(BigFloat& lhs, BigFloat& rhs) const {
    if (lhs.exponent_.GreaterThan(rhs.exponent_)) {
      rhs.mantissa_ = rhs.mantissa_.ShiftLeft(
          std::stoi(lhs.exponent_.Subtract(rhs.exponent_).ToString()));
      rhs.exponent_ = lhs.exponent_;
    } else if (lhs.exponent_.LessThan(rhs.exponent_)) {
      lhs.mantissa_ = lhs.mantissa_.ShiftLeft(
          std::stoi(rhs.exponent_.Subtract(lhs.exponent_).ToString()));
      lhs.exponent_ = rhs.exponent_;
    }
  }

private:
  BigInteger mantissa_;
  BigInteger exponent_;
  bool is_negative_;
};

}  // namespace pwrnum

#endif  // PWRNUM_BIG_FLOAT_BIG_FLOAT_H_
