import * as fs from 'fs';

export const env = (key: string): string => {
  const value = process.env[key];

  if (!value) {
    throw Error(`No environment variable found for ${key}`)
  }

  return value;
}

export const getJsonFromFile = <T = Record<string, string>>(path: string): T => {
  const buff = fs.readFileSync(`${process.cwd()}${path}`);
  const json = buff.toString();
  return JSON.parse(json);
}

export const envNumber = (key: string): number => {
  return Number(env[key]);
}
