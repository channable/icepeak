export {
  type Maybe,
  just,
  nothing,
  type Either,
  success,
  fail,
  parseArray,
  connect,
  timeoutPromise,
};
import type * as ws from "ws";

type Maybe<T> = { type: "Success"; value: T } | { type: "Fail" };

function just<T>(t: T): Maybe<T> {
  return { type: "Success", value: t };
}

function nothing<T>(): Maybe<T> {
  return { type: "Fail" };
}

function parseArray<T>(
  unknownArray: unknown[],
  parser: (u: unknown) => Maybe<T>,
): Maybe<T[]> {
  const tArray: T[] = [];
  for (const val of unknownArray) {
    const parsed = parser(val);
    if (parsed.type == "Fail") return nothing();
    tArray.push(parsed.value);
  }
  return just(tArray);
}

type Either<E, T> = { type: "Fail"; error: E } | { type: "Success"; value: T };

function success<E, T>(t: T): Either<E, T> {
  return { type: "Success", value: t };
}

function fail<E, T>(e: E): Either<E, T> {
  return { type: "Fail", error: e };
}
