#!#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: fmt.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 23:57:18
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-25 23:57:46
##

if (!requireNamespace("styler", quietly = TRUE)) {
  stop("Missing required package: styler. Install it with install.packages().")
}

#' Format R Code Files
styler::style_dir("R", recursive = TRUE)
