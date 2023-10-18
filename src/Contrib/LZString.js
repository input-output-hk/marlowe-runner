import LZString from 'lz-string';

export function decompressFromURI(uriComponent) {
    return LZString.decompressFromEncodedURIComponent(uriComponent);
}
