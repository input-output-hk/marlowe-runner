// TODO: Implement Bech32 encoding/decoding from cbor and from string
export class Bech32 {
  readonly value!: string;

  private constructor(s: string) {
    this.value = s;
  }

  static fromString(s: string): Bech32 {
    //TODO: validatate the value
    return new Bech32(s);
  }
}

