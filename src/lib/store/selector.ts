import { derived, get, type Readable, type Writable } from "svelte/store";

export const selector = <S extends Readable<unknown> | Readable<unknown>[] | [], T>(
  stores: S,
  getter: (
    values: S extends Readable<infer U>
      ? U
      : { [P in keyof S]: S[P] extends Readable<infer U> ? U : never },
  ) => T,
  setter: (value: T) => void,
): Writable<T> => {
  const derivedStore = derived<S, T>(stores, getter);
  return {
    subscribe: derivedStore.subscribe,
    set: setter,
    update(updater) {
      setter(updater(get(derivedStore)));
    },
  };
};
