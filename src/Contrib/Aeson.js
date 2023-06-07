export function parseImpl(resultHandlers, reviver, jsonStr) {
  try {
    return resultHandlers.success(JSON.parse(jsonStr, reviver));
  }
  catch (e) {
    return resultHandlers.failure(e.message);
  }
}
