export { createUseIcepeak, createCoreUseIcepeak  }

import * as icepeak_core from './icepeak-core.mjs';
import * as icepeak from './icepeak.mjs';

import { useState, useEffect } from 'react';

function createUseIcepeak(icepeakObject : icepeak.Icepeak) {

  function useIcepeak(path : string) {
    const [ icepeakPathValue, setIcepeakPathValue ]
      = useState<unknown>(null);

    useEffect(() => {
      const subscription = icepeakObject
	.subscribe(path, setIcepeakPathValue)
      return () => subscription.unsubscribe()
    })
    return icepeakPathValue
  }

  return useIcepeak
}

function createCoreUseIcepeak<TokenExtraData>(
  icepeakCoreObject : icepeak_core.IcepeakCore<any, TokenExtraData>,
) {

  function useIcepeak(path : string, tokenExtraData : TokenExtraData) {
    const [ icepeakPathValue, setIcepeakPathValue ]
      = useState<unknown>(null);

    useEffect(() => {
      const subscriptionRef
	= icepeakCoreObject.createSubscriptionRef(path)

      subscriptionRef.onUpdate(setIcepeakPathValue)
      subscriptionRef.subscribe(tokenExtraData)

      return () => subscriptionRef.unusubscribe()
    })
    return icepeakPathValue
  }

  return useIcepeak
}

