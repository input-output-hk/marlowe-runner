import LZString from 'lz-string';

export function decompressFromURIImpl(uriComponent) {
  try {
    return LZString.decompressFromEncodedURIComponent(uriComponent);
  } catch (e) {
    return null;
  }
}

export function compressToURI(str) {
  return LZString.compressToEncodedURIComponent(str);
}