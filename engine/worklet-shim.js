/*
 * Globals an AudioWorkletGlobalScope is missing that the Emscripten runtime
 * expects. This module exists separately, and must be imported before
 * surge-worklet.js, because ES module imports are hoisted: anything written
 * inline in the processor would run after the glue had already evaluated.
 *
 * Verified absent in Chrome's AudioWorkletGlobalScope: window, self,
 * WorkerGlobalScope, process, fetch, setTimeout, performance, crypto,
 * TextDecoder, TextEncoder.
 */

// Emscripten identifies its host by probing for window / WorkerGlobalScope /
// process. With none of them present it concludes "d8 shell" and selects shell
// code paths — notably a random_get that shells out to /dev/urandom through a
// `os` global that does not exist here. Declaring WorkerGlobalScope makes the
// detection agree with how the module is built (-sENVIRONMENT=web,worker,node).
// The worker branches it then picks (XMLHttpRequest / fetch for loading the
// wasm) are never reached, since the bytes arrive via processorOptions.
if (typeof globalThis.WorkerGlobalScope === "undefined") {
  globalThis.WorkerGlobalScope = function WorkerGlobalScope() {};
}

// Emscripten routes clock_gettime(CLOCK_MONOTONIC), and therefore
// std::chrono::steady_clock, through performance.now(). Surge calls
// steady_clock::now() while constructing SurgeStorage, so without this the
// engine dies in its constructor with only an "unreachable" trap to show for it.
// currentTime is the worklet's own monotonic clock, in seconds.
if (typeof globalThis.performance === "undefined") {
  globalThis.performance = {
    now: () => (typeof currentTime === "number" ? currentTime * 1000 : Date.now()),
  };
}

// Backs std::random_device, which Surge uses when constructing every voice's
// MSEG evaluator state. Not security-sensitive here: it seeds LFO/MSEG jitter.
if (typeof globalThis.crypto === "undefined") {
  globalThis.crypto = {
    getRandomValues: (arr) => {
      for (let i = 0; i < arr.length; i++) arr[i] = (Math.random() * 256) | 0;
      return arr;
    },
  };
}
