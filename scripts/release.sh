#!/usr/bin/env bash
set -euo pipefail

VERSION="${1:?Usage: $0 <version> (e.g. v0.1.0)}"

echo "==> Running tests..."
make test

echo "==> Building binary..."
make build

echo "==> Tagging ${VERSION}..."
git tag -a "${VERSION}" -m "Release ${VERSION}"
git push origin "${VERSION}"

ARCHIVE="hemlock-$(uname -s)-$(uname -m).tar.gz"
echo "==> Packaging ${ARCHIVE}..."
tar czf "${ARCHIVE}" -C bin .

echo "==> Creating GitHub release..."
gh release create "${VERSION}" "${ARCHIVE}" --title "${VERSION}" --generate-notes

rm -f "${ARCHIVE}"
echo "==> Done: ${VERSION}"
