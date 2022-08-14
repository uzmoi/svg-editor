import { writable } from "svelte/store";
import { clamp } from "emnorst";
import { Vec2 } from "../../lib/vec";

const MIN_SCALE = 100 / 96; // %
const MAX_SCALE = 12800; // %

export const scale = writable(100); // %
export const translate = writable(Vec2.square(0)); // px

export const changeScale = (isEnlarge: boolean, offset: Vec2<number>) => {
  scale.update(scale => {
    const isNormal = isEnlarge ? scale >= 100.0 : scale > 100.0;
    // 拡大率100%以下は逆数にする
    // ただし66.666...とかが少数でmod 3できないので200
    const x = Math.floor(isNormal ? scale : 200.0 / scale);
    const rate = (x % 3 === 0) === (isNormal === isEnlarge) ? 4 / 3 : 3 / 2;
    const y = isEnlarge ? scale * rate : scale / rate;
    const newScale = clamp(y, MIN_SCALE, MAX_SCALE);
    const deltaTranslate = offset.sub(offset.map(x => x * (newScale / scale)));
    translate.update(translate => translate.add(deltaTranslate));
    return newScale;
  });
};
