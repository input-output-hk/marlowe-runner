/** @type {(fr: File) => Promise<string | null>} */
export const _loadFile = (file) =>
  new Promise((k) => {
    const fr = new FileReader();
    fr.onload = (e) => {
      /** @type {string | ArrayBuffer | null | undefined} */
      const result = e.target?.result;
      if (typeof result === "string") k(result);
      // TODO handle array buffer case?
      else k(null);
    };
    fr.readAsText(file);
  });
