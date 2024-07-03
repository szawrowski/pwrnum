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

namespace cait {

class bigfloat_t {
public:
  bigfloat_t() : exponent_("0"), is_negative_(false) {}

  bigfloat_t(const std::string& number) {
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
        mantissa_ = bigint_t(int_part);
      } else {
        mantissa_ = bigint_t("0");
      }
      if (!frac_part.empty()) {
        exponent_ =
            bigint_t(std::to_string(-static_cast<int>(frac_part.size())));
        mantissa_ =
            mantissa_
                .multiply(bigint_t("1" + std::string(frac_part.size(), '0')))
                .add(bigint_t(frac_part));
      } else {
        exponent_ = bigint_t("0");
      }
    } else {
      mantissa_ = bigint_t(number.substr(start));
      exponent_ = bigint_t("0");
    }
    remove_leading_zeros();
  }

  bigfloat_t(const bigfloat_t& other)
      : mantissa_(other.mantissa_),
        exponent_(other.exponent_),
        is_negative_(other.is_negative_) {}

  bigfloat_t(bigfloat_t&& other) noexcept
      : mantissa_(std::move(other.mantissa_)),
        exponent_(std::move(other.exponent_)),
        is_negative_(other.is_negative_) {
    other.is_negative_ = false;
  }

  bigfloat_t& operator=(const bigfloat_t& other) {
    if (this != &other) {
      mantissa_ = other.mantissa_;
      exponent_ = other.exponent_;
      is_negative_ = other.is_negative_;
    }
    return *this;
  }

  bigfloat_t& operator=(bigfloat_t&& other) noexcept {
    if (this != &other) {
      mantissa_ = std::move(other.mantissa_);
      exponent_ = std::move(other.exponent_);
      is_negative_ = other.is_negative_;
      other.is_negative_ = false;
    }
    return *this;
  }

  [[nodiscard]] bigfloat_t add(const bigfloat_t& other) const {
    bigfloat_t lhs = *this;
    bigfloat_t rhs = other;

    normalize(lhs, rhs);

    bigfloat_t result;
    result.mantissa_ = lhs.mantissa_.add(rhs.mantissa_);
    result.exponent_ = lhs.exponent_;
    result.is_negative_ = lhs.is_negative_;
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigfloat_t subtract(const bigfloat_t& other) const {
    bigfloat_t lhs = *this;
    bigfloat_t rhs = other;

    normalize(lhs, rhs);

    bigfloat_t result;
    result.mantissa_ = lhs.mantissa_.subtract(rhs.mantissa_);
    result.exponent_ = lhs.exponent_;
    result.is_negative_ = lhs.is_negative_;
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigfloat_t multiply(const bigfloat_t& other) const {
    bigfloat_t result;
    result.mantissa_ = mantissa_.multiply(other.mantissa_);
    result.exponent_ = exponent_.add(other.exponent_);
    result.is_negative_ = is_negative_ != other.is_negative_;
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] bigfloat_t divide(const bigfloat_t& other) const {
    if (other.equal(bigfloat_t("0"))) {
      throw std::invalid_argument("Division by zero");
    }

    bigfloat_t result;
    result.mantissa_ = mantissa_.divide(other.mantissa_);
    result.exponent_ = exponent_.subtract(other.exponent_);
    result.is_negative_ = is_negative_ != other.is_negative_;
    result.remove_leading_zeros();
    return result;
  }

  [[nodiscard]] int compare(const bigfloat_t& other) const {
    bigfloat_t lhs = *this;
    bigfloat_t rhs = other;

    normalize(lhs, rhs);

    return lhs.mantissa_.compare(rhs.mantissa_);
  }

  [[nodiscard]] bool less_than(const bigfloat_t& other) const {
    return compare(other) < 0;
  }

  [[nodiscard]] bool greater_than(const bigfloat_t& other) const {
    return compare(other) > 0;
  }

  [[nodiscard]] bool equal(const bigfloat_t& other) const {
    return compare(other) == 0;
  }

  [[nodiscard]] bigfloat_t abs() const {
    bigfloat_t result = *this;
    result.is_negative_ = false;
    return result;
  }

  [[nodiscard]] bigfloat_t pow(int exponent) const {
    if (exponent < 0) {
      throw std::invalid_argument("Negative exponent not supported");
    }

    bigfloat_t result("1");
    bigfloat_t base = *this;

    while (exponent > 0) {
      if (exponent % 2 == 1) {
        result = result.multiply(base);
      }
      base = base.multiply(base);
      exponent /= 2;
    }

    return result;
  }

  [[nodiscard]] bigfloat_t sqr() const { return this->multiply(*this); }

  [[nodiscard]] bigfloat_t sqrt() const {
    if (is_negative_) {
      throw std::invalid_argument("Square root of negative number");
    }

    if (this->equal(bigfloat_t("0")) || this->equal(bigfloat_t("1"))) {
      return *this;
    }

    bigfloat_t low("1"), high = *this;
    while (low.less_than(high) || low.equal(high)) {
      auto mid = low.add(high).divide(bigfloat_t("2"));
      auto squared = mid.sqr();
      if (squared.equal(*this)) {
        return mid;
      }
      if (squared.less_than(*this)) {
        low = mid.add(bigfloat_t("1"));
      } else {
        high = mid.subtract(bigfloat_t("1"));
      }
    }

    return high;
  }

  void swap(bigfloat_t& other) {
    std::swap(mantissa_, other.mantissa_);
    std::swap(exponent_, other.exponent_);
    std::swap(is_negative_, other.is_negative_);
  }

  [[nodiscard]] bool is_positive() const {
    return !is_negative_ && !equal(bigfloat_t("0"));
  }

  [[nodiscard]] bool is_negative() const { return is_negative_; }

  [[nodiscard]] std::string to_string() const {
    std::ostringstream oss;
    if (is_negative_ && !mantissa_.equal(bigint_t("0"))) {
      oss << "-";
    }

    const std::string mantissa_str = mantissa_.to_string();
    const int exp_val = std::stoi(exponent_.to_string());
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
  void remove_leading_zeros() {
    mantissa_.remove_leading_zeros();
    if (mantissa_.equal(bigint_t("0"))) {
      exponent_ = bigint_t("0");
      is_negative_ = false;
    }
  }

  void normalize(bigfloat_t& lhs, bigfloat_t& rhs) const {
    if (lhs.exponent_.greater_than(rhs.exponent_)) {
      rhs.mantissa_ = rhs.mantissa_.shift_left(
          std::stoi(lhs.exponent_.subtract(rhs.exponent_).to_string()));
      rhs.exponent_ = lhs.exponent_;
    } else if (lhs.exponent_.less_than(rhs.exponent_)) {
      lhs.mantissa_ = lhs.mantissa_.shift_left(
          std::stoi(rhs.exponent_.subtract(lhs.exponent_).to_string()));
      lhs.exponent_ = rhs.exponent_;
    }
  }

private:
  bigint_t mantissa_;
  bigint_t exponent_;
  bool is_negative_;
};

}  // namespace pwrnum

#endif  // PWRNUM_BIG_FLOAT_BIG_FLOAT_H_
