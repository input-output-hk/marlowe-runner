export const waitFor = async <T>(
  predicate: () => T | Promise<T>,
  options?: {timeout?:number; wait?: number, label?: string}
): Promise<T> => {
  const { timeout = 60000, wait=200 } = options || {};
  const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));
  const startDate = new Date();

  while (new Date().getTime() - startDate.getTime() < timeout) {
    const result = await predicate();
    if (result) {
      return result;
    }

    await sleep(wait)
    if(!options?.label) {
      console.log(`Waiting ${wait}ms`);
    } else {
      console.log(`Waiting ${wait}ms for ${options.label}`);
    }
  }

  throw new Error(`Wait time of ${timeout}ms exceeded`);
}
