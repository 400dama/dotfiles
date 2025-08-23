#!/usr/bin/env bash
set -e  # 遇到错误就退出

# 获取脚本所在目录（相当于 Python 的 __file__）
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

EMACS_SOURCE="$SCRIPT_DIR/damacs"
EMACS_TARGET="$HOME/.emacs.d"


if [ -e "$EMACS_TARGET" ] || [ -L "$EMACS_TARGET" ]; then
    echo "⚠️  $EMACS_TARGET 已存在"
    rm -rf "$EMACS_TARGET"
fi
# 创建符号链接
ln -s "$EMACS_SOURCE" "$EMACS_TARGET"
echo "✅ 已创建链接: $EMACS_TARGET -> $EMACS_SOURCE"