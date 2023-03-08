{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Use camelCase" -}


module Test where

import qualified Data.ByteString.Short                         as DataByteStringShort
import qualified Data.ByteString.Lazy                          as DataByteStringLazy
import qualified Codec.Serialise                               as CodecSerialise
import qualified Data.Maybe                                    as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                                    as LedgerAda
import qualified Ledger.Address                                as LedgerAddress
import qualified Ledger.Value                                  as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts                as UtilsScriptsV2
import qualified Plutus.V1.Ledger.ProtocolVersions             as LedgerProtocolVersionsV1
import qualified Plutus.V1.Ledger.Scripts                      as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                     as LedgerContextsV2
import qualified Plutus.V2.Ledger.EvaluationContext            as LedgerEvaluationContextV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as TxAssocMap
import           PlutusTx.Prelude
import qualified Prelude                                       as P
import qualified PlutusTx.Builtins                             as TxBuiltins

---------------------------------------------------

{-# INLINABLE flattenMapDeleteZeros #-}
flattenMapDeleteZeros :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenMapDeleteZeros !mp =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]
    in
        f3

---------------------------------------------------

{-# INLINABLE flattenAndSortMapDeleteZeros #-}
flattenAndSortMapDeleteZeros :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenAndSortMapDeleteZeros !mp =
    let
        sort_CurrencySymbol :: (LedgerApiV2.CurrencySymbol, a) -> (LedgerApiV2.CurrencySymbol, a) -> Ordering
        sort_CurrencySymbol (!cs1, _) (!cs2, _) = compare cs1 cs2
        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
        !f1 = sortBy sort_CurrencySymbol (TxAssocMap.toList mp)
        !f2 = [ ( cs ,  sortBy sort_TokenName  (TxAssocMap.toList mp')) | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]
    in
        f3

---------------------------------------------------

{-# INLINABLE flattenValueAndDeleteZeros #-}
flattenValueAndDeleteZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValueAndDeleteZeros (LedgerValue.Value !mp) = flattenMapDeleteZeros mp

---------------------------------------------------

{-# INLINABLE flattenAndSortValueAndDeleteZeros #-}
flattenAndSortValueAndDeleteZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenAndSortValueAndDeleteZeros  (LedgerValue.Value !mp) = flattenAndSortMapDeleteZeros mp

---------------------------------------------------

{-# INLINABLE sortFlattenedValue #-}
sortFlattenedValue :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
sortFlattenedValue !xs =
    let
        sort_CurrencySymbolAndTokenName :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, a) -> (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, a) -> Ordering
        sort_CurrencySymbolAndTokenName (!cs1, !tn1, _) (!cs2, !tn2, _) =
            case compare cs1 cs2 of
                EQ -> compare tn1 tn2
                x -> x
        !f1 = sortBy sort_CurrencySymbolAndTokenName xs
    in
        f1

---------------------------------------------------

{-# INLINABLE normalizeValue #-}
normalizeValue :: LedgerApiV2.Value -> LedgerApiV2.Value
normalizeValue = LedgerApiV2.Value . TxAssocMap.fromList . sort' . filterRange (/=TxAssocMap.empty)
               . mapRange normalizeTokenMap . TxAssocMap.toList . LedgerApiV2.getValue
  where normalizeTokenMap = TxAssocMap.fromList . sort' . filterRange (/=0) . TxAssocMap.toList
        filterRange p kvs = [(k,v) | (k,v) <- kvs, p v]
        mapRange f xys = [(x,f y) | (x,y) <- xys]
        sort' xs = sortBy compare xs

---------------------------------------------------

{-# INLINABLE flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros #-}
flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros [] [] = True
flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros ((!cs1, !tn1, !amt1):(!xs1)) ((!cs2, !tn2, !amt2):(!xs2))
    | cs1 == cs2 && tn1 == tn2 && amt1 == amt2 = flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros xs1 xs2
    | otherwise =
        let
            flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros' :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
            flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros' (!cs1', !tn1', !amt1') !xs1' !xs2' ((!cs2', !tn2', !amt2'):(!xs3'))
                | cs1' == cs2' && tn1' == tn2' && amt1' == amt2' = flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros xs1' (xs2'++xs3')
                | otherwise = flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros' (cs1', tn1', amt1') xs1' ((cs2', tn2', amt2'):xs2') xs3'
            flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros' _ _ _ _ = False
        in
            flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros' (cs1, tn1, amt1) xs1 [(cs2, tn2, amt2)] xs2
flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros _ _ = False

---------------------------------------------------

{-# INLINABLE listTNEqualsListTN #-}
listTNEqualsListTN :: [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool

listTNEqualsListTN [] [] = True

listTNEqualsListTN [] ((_, !am2):(!xs2)) =
    am2 == 0 && listTNEqualsListTN [] xs2

listTNEqualsListTN ((_, !am1):(!xs1)) [] =
    am1 == 0 && listTNEqualsListTN xs1 []

listTNEqualsListTN ((!tn1, !am1):(!xs1)) ((!tn2, !am2):(!xs2))
    | am1 == 0 =
        listTNEqualsListTN xs1 ((tn2, am2):xs2)

    | am2 == 0 =
        listTNEqualsListTN ((tn1, am1):xs1) xs2

    | tn1 == tn2 && am1 == am2 =
        listTNEqualsListTN xs1 xs2

    | otherwise =
        let
            listTNEqualsListTN' :: (LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
            listTNEqualsListTN' (!tn1', !am1') !xs1' !xs2' ((!tn2', !am2'):(!xs3'))
                | am2' == 0                     = listTNEqualsListTN' (tn1', am1') xs1' xs2' xs3'
                | tn1' == tn2' && am1' == am2'  = listTNEqualsListTN xs1' (xs2'++xs3')
                | otherwise                     = listTNEqualsListTN' (tn1', am1') xs1' ((tn2', am2'):xs2') xs3'
            listTNEqualsListTN' _ _ _ _ = False
        in
            listTNEqualsListTN' (tn1, am1) xs1 [(tn2, am2)] xs2

listTNEqualsListTN _ _ = False

---------------------------------------------------

{-# INLINABLE listCSEqualsListCS #-}
listCSEqualsListCS :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
listCSEqualsListCS [] [] = True

listCSEqualsListCS ((_, !mp1):(!xs1)) [] =
        let
            !listTN1 = TxAssocMap.toList mp1
        in
            listTNEqualsListTN listTN1 [] && listCSEqualsListCS xs1 []

listCSEqualsListCS [] ((_, !mp2):(!xs2)) =
        let
            !listTN2 = TxAssocMap.toList mp2
        in
            listTNEqualsListTN [] listTN2 && listCSEqualsListCS [] xs2

listCSEqualsListCS ((!cs1, !mp1):(!xs1)) ((!cs2, !mp2):(!xs2))

    | cs1 == cs2 =
        let
            !listTN1 = TxAssocMap.toList mp1
            !listTN2 = TxAssocMap.toList mp2
        in
            listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS xs1 xs2

    | otherwise =
        let
            listCSEqualsListCS' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool

            listCSEqualsListCS' (!_, !mp1') !xs1' !xs2' [] =
                let
                    !listTN1 = TxAssocMap.toList mp1'
                in
                    listTNEqualsListTN listTN1 [] && listCSEqualsListCS xs1' xs2'

            listCSEqualsListCS' (!cs1', !mp1') !xs1' !xs2' ((!cs2', !mp2'):xs3') =
                if cs1' == cs2' then
                    let
                        !listTN1 = TxAssocMap.toList mp1'
                        !listTN2 = TxAssocMap.toList mp2'
                    in
                        listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS xs1' (xs2'++xs3')
                else
                    listCSEqualsListCS' (cs1', mp1') xs1' ((cs2', mp2'):xs2') xs3'

            listCSEqualsListCS' _ _ _ _ = False
        in
            listCSEqualsListCS' (cs1, mp1) xs1 [(cs2, mp2)] xs2

listCSEqualsListCS _ _ = False

---------------------------------------------------

{-# INLINABLE listCSEqualsListCS'Sorted #-}
listCSEqualsListCS'Sorted :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
listCSEqualsListCS'Sorted [] [] = True

listCSEqualsListCS'Sorted ((_, !mp1):(!xs1)) [] =
        let
            sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
            sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2

            !listTN1 = sortBy sort_TokenName (TxAssocMap.toList mp1)
        in
            listTNEqualsListTN listTN1 [] && listCSEqualsListCS'Sorted xs1 []

listCSEqualsListCS'Sorted [] ((_, !mp2):(!xs2)) =
        let
            sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
            sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2

            !listTN2 = sortBy sort_TokenName (TxAssocMap.toList mp2)
        in
            listTNEqualsListTN [] listTN2 && listCSEqualsListCS'Sorted [] xs2

listCSEqualsListCS'Sorted ((!cs1, !mp1):(!xs1)) ((!cs2, !mp2):(!xs2))

    | cs1 == cs2 =
        let
            sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
            sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2

            !listTN1 = sortBy sort_TokenName (TxAssocMap.toList mp1)
            !listTN2 = sortBy sort_TokenName (TxAssocMap.toList mp2)
        in
            listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS'Sorted xs1 xs2

    | otherwise =
        let
            listCSEqualsListCS'Sorted' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool

            listCSEqualsListCS'Sorted' (!_, !mp1') !xs1' !xs2' [] =
                let
                    !listTN1 = TxAssocMap.toList mp1'
                in
                    listTNEqualsListTN listTN1 [] && listCSEqualsListCS'Sorted xs1' xs2'

            listCSEqualsListCS'Sorted' (!cs1', !mp1') !xs1' !xs2' ((!cs2', !mp2'):xs3') =
                if cs1' == cs2' then
                    let
                        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
                        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2

                        !listTN1 = sortBy sort_TokenName (TxAssocMap.toList mp1')
                        !listTN2 = sortBy sort_TokenName (TxAssocMap.toList mp2')
                    in
                        listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS'Sorted xs1' (xs2'++xs3')
                else
                    listCSEqualsListCS'Sorted' (cs1', mp1') xs1' ((cs2', mp2'):xs2') xs3'

            listCSEqualsListCS'Sorted' _ _ _ _ = False
        in
            listCSEqualsListCS'Sorted' (cs1, mp1) xs1 [(cs2, mp2)] xs2

listCSEqualsListCS'Sorted _ _ = False

---------------------------------------------------

{-# INLINABLE listCSEqualsListCS'SmartSorted #-}
listCSEqualsListCS'SmartSorted :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
listCSEqualsListCS'SmartSorted [] [] = True

listCSEqualsListCS'SmartSorted ((_, !mp1):(!xs1)) [] =
        let
            !listTN1 = TxAssocMap.toList mp1

            !listTN1' =
                if length listTN1 > 3 then
                    let
                        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
                        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
                    in
                        sortBy sort_TokenName listTN1
                else
                    listTN1
        in
            listTNEqualsListTN listTN1' [] && listCSEqualsListCS'SmartSorted xs1 []

listCSEqualsListCS'SmartSorted [] ((_, !mp2):(!xs2)) =
        let
            !listTN2 = TxAssocMap.toList mp2

            !listTN2' =
                if length listTN2 > 3 then
                    let
                        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
                        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
                    in
                        sortBy sort_TokenName listTN2
                else
                    listTN2
        in
            listTNEqualsListTN [] listTN2' && listCSEqualsListCS'SmartSorted [] xs2

listCSEqualsListCS'SmartSorted ((!cs1, !mp1):(!xs1)) ((!cs2, !mp2):(!xs2))

    | cs1 == cs2 =
        let
            !listTN1 = TxAssocMap.toList mp1
            !listTN2 = TxAssocMap.toList mp2

            !(listTN1', listTN2') =
                if length listTN1 > 3 then
                    let
                        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
                        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
                    in
                        (sortBy sort_TokenName listTN1, sortBy sort_TokenName listTN2)
                else
                    (listTN1, listTN2)
        in
            listTNEqualsListTN listTN1' listTN2' && listCSEqualsListCS'SmartSorted xs1 xs2

    | otherwise =
        let
            listCSEqualsListCS'SmartSorted' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool

            listCSEqualsListCS'SmartSorted' (!_, !mp1') !xs1' !xs2' [] =
                let
                    !listTN1 = TxAssocMap.toList mp1'
                in
                    listTNEqualsListTN listTN1 [] && listCSEqualsListCS'SmartSorted xs1' xs2'

            listCSEqualsListCS'SmartSorted' (!cs1', !mp1') !xs1' !xs2' ((!cs2', !mp2'):xs3') =
                if cs1' == cs2' then
                    let
                        !listTN1 = TxAssocMap.toList mp1'
                        !listTN2 = TxAssocMap.toList mp2'

                        !(listTN1', listTN2') =
                            if length listTN1 > 3 then
                                let
                                    sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
                                    sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
                                in
                                    (sortBy sort_TokenName listTN1, sortBy sort_TokenName listTN2)
                            else
                                (listTN1, listTN2)
                    in
                        listTNEqualsListTN listTN1' listTN2' && listCSEqualsListCS'SmartSorted xs1' (xs2'++xs3')
                else
                    listCSEqualsListCS'SmartSorted' (cs1', mp1') xs1' ((cs2', mp2'):xs2') xs3'

            listCSEqualsListCS'SmartSorted' _ _ _ _ = False
        in
            listCSEqualsListCS'SmartSorted' (cs1, mp1) xs1 [(cs2, mp2)] xs2

listCSEqualsListCS'SmartSorted _ _ = False

---------------------------------------------------

{-# INLINABLE sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros #-}
sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros !sortedFlattenValue1 !sortedFlattenValue2 =
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData sortedFlattenValue1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData sortedFlattenValue2)

---------------------------------------------------

{-# INLINABLE normalizedValueEqualsNormalizedValue #-}
normalizedValueEqualsNormalizedValue ::LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
normalizedValueEqualsNormalizedValue !normalizedValue1 !normalizedValue2 =
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData normalizedValue1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData normalizedValue2)

---------------------------------------------------

{-# INLINABLE valueEqualsValue1 #-}
{- Convierte los values a una lista [(cs, tn, am)] y luego las compara, eliminando los elementos a medida que los encuentra -}
valueEqualsValue1 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue1 !value1 !value2 =
    let
        !flattenedValue1 = flattenValueAndDeleteZeros value1
        !flattenedValue2 = flattenValueAndDeleteZeros value2
    in
        flattenedValue1 `flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros` flattenedValue2

{-# INLINABLE valueEqualsValue1'Sorted #-}
{- Convierte los values a una lista ordenada [(cs, tn, am)] y luego las compara, eliminando los elementos a medida que los encuentra -}
valueEqualsValue1'Sorted :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue1'Sorted !value1 !value2 =
    let
        !flattenedValue1 = flattenAndSortValueAndDeleteZeros value1
        !flattenedValue2 = flattenAndSortValueAndDeleteZeros value2
    in
        flattenedValue1 `flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros` flattenedValue2

---------------------------------------------------

{-# INLINABLE valueEqualsValue2 #-}
{- Convierte los values a una lista [(cs, tn, am)] para comparar la cantidad de elementos y luego usa el builtin LedgerValue.assetClassValueOf para buscar los elementos de la lista en el value -}
valueEqualsValue2 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue2 !value1 !value2 =
    let
        !flattenedValue1 = flattenValueAndDeleteZeros value1
        !flattenedValue2 = flattenValueAndDeleteZeros value2
    in
        length flattenedValue1 == length flattenedValue2 &&
        all (\(!cs, !tn, !amount) ->
            let
                !ac = LedgerValue.AssetClass (cs, tn)
            in
                LedgerValue.assetClassValueOf value2 ac == amount
        ) flattenedValue1

---------------------------------------------------

{-# INLINABLE valueEqualsValue3 #-}
valueEqualsValue3 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue3 (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        !flattenedValue1 = flattenMapDeleteZeros mp1
        !flattenedValue2 = flattenMapDeleteZeros mp2

        valueOfCSAndTNInMap :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> Integer
        valueOfCSAndTNInMap !mpCS !cur !tn =
            case TxAssocMap.lookup cur mpCS of
                Nothing     -> 0
                Just mapTN  -> DataMaybe.fromMaybe 0 (TxAssocMap.lookup tn mapTN)
    in
        length flattenedValue1 == length flattenedValue2 &&
        all (\(cs, tn, amount) -> valueOfCSAndTNInMap mp2 cs tn == amount) flattenedValue1

{-# INLINABLE valueEqualsValue3'Sorted #-}
valueEqualsValue3'Sorted :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue3'Sorted (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        !flattenedValue1 = flattenAndSortMapDeleteZeros mp1
        !flattenedValue2 = flattenAndSortMapDeleteZeros mp2

        valueOfCSAndTNInMap :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> Integer
        valueOfCSAndTNInMap !mpCS !cur !tn =
            case TxAssocMap.lookup cur mpCS of
                Nothing     -> 0
                Just mapTN  -> DataMaybe.fromMaybe 0 (TxAssocMap.lookup tn mapTN)
    in
        length flattenedValue1 == length flattenedValue2 &&
        all (\(cs, tn, amount) -> valueOfCSAndTNInMap mp2 cs tn == amount) flattenedValue1

---------------------------------------------------

{-# INLINABLE valueEqualsValue4 #-}
valueEqualsValue4 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue4 (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        !listCS1 = TxAssocMap.toList mp1
        !listCS2 = TxAssocMap.toList mp2
    in
        listCS1 `listCSEqualsListCS` listCS2

---------------------------------------------------

{-# INLINABLE valueEqualsValue4'Sorted #-}
valueEqualsValue4'Sorted :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue4'Sorted (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        sort_CurrencySymbol :: (LedgerApiV2.CurrencySymbol, a) -> (LedgerApiV2.CurrencySymbol, a) -> Ordering
        sort_CurrencySymbol (!cs1, _) (!cs2, _) = compare cs1 cs2

        !listCS1 = sortBy sort_CurrencySymbol (TxAssocMap.toList mp1)
        !listCS2 = sortBy sort_CurrencySymbol (TxAssocMap.toList mp2)
    in
        listCS1 `listCSEqualsListCS'Sorted` listCS2

---------------------------------------------------

{-# INLINABLE valueEqualsValue4'SmartSorted #-}
valueEqualsValue4'SmartSorted :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue4'SmartSorted (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        sort_CurrencySymbol :: (LedgerApiV2.CurrencySymbol, a) -> (LedgerApiV2.CurrencySymbol, a) -> Ordering
        sort_CurrencySymbol (!cs1, _) (!cs2, _) = compare cs1 cs2

        !listCS1 = TxAssocMap.toList mp1
        !listCS2 = TxAssocMap.toList mp2

        (!listCS1',!listCS2') =
            if length listCS1 > 3 then
                (sortBy sort_CurrencySymbol listCS1, sortBy sort_CurrencySymbol listCS2)
            else
                (listCS1, listCS2)

    in
        listCS1' `listCSEqualsListCS'SmartSorted` listCS2'

---------------------------------------------------

{-# INLINABLE valueEqualsValue5 #-}
valueEqualsValue5 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue5 !value1 !value2 =
   let
        !sortedFlattenedValue1 = flattenAndSortValueAndDeleteZeros value1
        !sortedFlattenedValue2 = flattenAndSortValueAndDeleteZeros value2
    in
        sortedFlattenedValue1 `sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros` sortedFlattenedValue2

---------------------------------------------------

{-# INLINABLE valueEqualsValue6 #-}
valueEqualsValue6 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue6 !value1 !value2 =
    let
        !normalizedValue1 = normalizeValue value1
        !normalizedValue2 = normalizeValue value2
    in
        normalizedValue1 `normalizedValueEqualsNormalizedValue` normalizedValue2

---------------------------------------------------

{-# INLINABLE valueEqualsValue7 #-}
valueEqualsValue7 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue7 !value1 !value2 =
    value1 == value2

---------------------------------------------------

-- {-# INLINABLE valueEqualsValue6 #-}
-- valueEqualsValue6 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
-- valueEqualsValue6 !value1 !value2 =
--     let
--         !flattenedValue1 = flattenValueAndDeleteZeros value1
--     in
--         if length flattenedValue1 > 15 then
--             let
--                 !sortedFlattenedValue1 = sortFlattenedValue flattenedValue1
--                 !sortedFlattenedValue2 = flattenAndSortValueAndDeleteZeros value2
--             in
--                 sortedFlattenedValue1 `sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros` sortedFlattenedValue2
--         else
--             let
--                 !flattenedValue2 = flattenValueAndDeleteZeros value2
--             in
--                 flattenedValue1 `flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros` flattenedValue2

---------------------------------------------------

{-# INLINABLE mkPolicy1 #-}
mkPolicy1 :: BuiltinData -> BuiltinData -> ()
mkPolicy1 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        -- !value_For_Control = value1 <> value2 <> value3 <> value4 <> valueADA
        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue1` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy1'Sorted #-}
mkPolicy1'Sorted :: BuiltinData -> BuiltinData -> ()
mkPolicy1'Sorted _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        -- !value_For_Control = value1 <> value2 <> value3 <> value4 <> valueADA
        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue1'Sorted` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy2 #-}
mkPolicy2 :: BuiltinData -> BuiltinData -> ()
mkPolicy2 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue2` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy3 #-}
mkPolicy3 :: BuiltinData -> BuiltinData -> ()
mkPolicy3 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error1" (value_For_Control `valueEqualsValue3` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy3'Sorted #-}
mkPolicy3'Sorted :: BuiltinData -> BuiltinData -> ()
mkPolicy3'Sorted _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error1" (value_For_Control `valueEqualsValue3'Sorted` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy4 #-}
mkPolicy4 :: BuiltinData -> BuiltinData -> ()
mkPolicy4 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue4` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy4'Sorted #-}
mkPolicy4'Sorted :: BuiltinData -> BuiltinData -> ()
mkPolicy4'Sorted _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue4'Sorted` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy4'SmartSorted #-}
mkPolicy4'SmartSorted :: BuiltinData -> BuiltinData -> ()
mkPolicy4'SmartSorted _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue4'SmartSorted` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy5 #-}
mkPolicy5 :: BuiltinData -> BuiltinData -> ()
mkPolicy5 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue5` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy6 #-}
mkPolicy6 :: BuiltinData -> BuiltinData -> ()
mkPolicy6 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue6` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINABLE mkPolicy7 #-}
mkPolicy7 :: BuiltinData -> BuiltinData -> ()
mkPolicy7 _ !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value_Real = outputs_Values!!0

        !value1 = inputs_Values!!0

        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        !value_For_Control =  valueADA <> value4 <> value3 <> value2 <> value1
    in
       if
            traceIfFalse "error" (value_For_Control `valueEqualsValue7` value_Real)
        then ()
        else error ()

---------------------------------------------------

{-# INLINEABLE policy1 #-}
policy1 :: LedgerApiV2.MintingPolicy
policy1 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy1 ||])

{-# INLINEABLE policy1'Sorted #-}
policy1'Sorted :: LedgerApiV2.MintingPolicy
policy1'Sorted = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy1'Sorted ||])

{-# INLINEABLE policy2 #-}
policy2 :: LedgerApiV2.MintingPolicy
policy2 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy2 ||])

{-# INLINEABLE policy3 #-}
policy3 :: LedgerApiV2.MintingPolicy
policy3 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy3 ||])

{-# INLINEABLE policy3'Sorted #-}
policy3'Sorted :: LedgerApiV2.MintingPolicy
policy3'Sorted = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy3'Sorted ||])

{-# INLINEABLE policy4 #-}
policy4 :: LedgerApiV2.MintingPolicy
policy4 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy4 ||])

{-# INLINEABLE policy4'Sorted #-}
policy4'Sorted :: LedgerApiV2.MintingPolicy
policy4'Sorted = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy4'Sorted ||])

{-# INLINEABLE policy4'SmartSorted #-}
policy4'SmartSorted :: LedgerApiV2.MintingPolicy
policy4'SmartSorted = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy4'SmartSorted ||])

{-# INLINEABLE policy5 #-}
policy5 :: LedgerApiV2.MintingPolicy
policy5 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy5 ||])

{-# INLINEABLE policy6 #-}
policy6 :: LedgerApiV2.MintingPolicy
policy6 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy6 ||])

{-# INLINEABLE policy7 #-}
policy7 :: LedgerApiV2.MintingPolicy
policy7 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy7 ||])

---------------------------------------------------

{-# INLINEABLE policy1_fromPlutonomy #-}
policy1_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy1_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy1 ||])


{-# INLINEABLE policy1'Sorted_fromPlutonomy #-}
policy1'Sorted_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy1'Sorted_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy1'Sorted ||])

{-# INLINEABLE policy2_fromPlutonomy #-}
policy2_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy2_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy2 ||])

{-# INLINEABLE policy3_fromPlutonomy #-}
policy3_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy3_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy3 ||])

{-# INLINEABLE policy3'Sorted_fromPlutonomy #-}
policy3'Sorted_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy3'Sorted_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy3'Sorted ||])

{-# INLINEABLE policy4_fromPlutonomy #-}
policy4_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy4_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy4 ||])

{-# INLINEABLE policy4'Sorted_fromPlutonomy #-}
policy4'Sorted_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy4'Sorted_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy4'Sorted ||])

{-# INLINEABLE policy4'SmartSorted_fromPlutonomy #-}
policy4'SmartSorted_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy4'SmartSorted_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy4'SmartSorted ||])

{-# INLINEABLE policy5_fromPlutonomy #-}
policy5_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy5_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy5 ||])

{-# INLINEABLE policy6_fromPlutonomy #-}
policy6_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy6_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy6 ||])

{-# INLINEABLE policy7_fromPlutonomy #-}
policy7_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy7_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy5 ||])

---------------------------------------------------

curSymbol :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol

---------------------------------------------------

evaluate :: P.IO ()
evaluate =
    let
        !value_Tk01 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk01")) 1
        !value_Tk02 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk02")) 1
        !value_Tk03 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk03")) 1
        !value_Tk04 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk04")) 1
        !value_Tk05 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk05")) 1
        !value_Tk06 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk06")) 1
        !value_Tk07 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk07")) 1
        !value_Tk08 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk08")) 1
        !value_Tk09 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk09")) 1
        !value_Tk10 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk10")) 1
        !value_Tk11 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3", LedgerApiV2.TokenName "tk11")) 1
        !value_Tk12 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3", LedgerApiV2.TokenName "tk12")) 1
        !value_Tk13 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3", LedgerApiV2.TokenName "tk13")) 1
        !value_Tk14 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3", LedgerApiV2.TokenName "tk14")) 1
        !value_Tk15 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3", LedgerApiV2.TokenName "tk15")) 1
        !value_Tk16 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", LedgerApiV2.TokenName "tk16")) 1
        !value_Tk17 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", LedgerApiV2.TokenName "tk17")) 1
        !value_Tk18 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", LedgerApiV2.TokenName "tk18")) 1
        !value_Tk19 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", LedgerApiV2.TokenName "tk19")) 1
        !value_Tk20 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", LedgerApiV2.TokenName "tk20")) 1
        !value_Tk21 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5", LedgerApiV2.TokenName "tk21")) 1
        !value_Tk22 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5", LedgerApiV2.TokenName "tk22")) 1
        !value_Tk23 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5", LedgerApiV2.TokenName "tk23")) 1
        !value_Tk24 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5", LedgerApiV2.TokenName "tk24")) 1
        !value_Tk25 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5", LedgerApiV2.TokenName "tk25")) 1
        !value_Tk26 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa6", LedgerApiV2.TokenName "tk26")) 1
        !value_Tk27 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa6", LedgerApiV2.TokenName "tk27")) 1
        !value_Tk28 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa6", LedgerApiV2.TokenName "tk28")) 1
        !value_Tk29 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa6", LedgerApiV2.TokenName "tk29")) 1
        !value_Tk30 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa6", LedgerApiV2.TokenName "tk30")) 1
        !value_Tk31 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa7", LedgerApiV2.TokenName "tk31")) 1
        !value_Tk32 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa7", LedgerApiV2.TokenName "tk32")) 1
        !value_Tk33 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa7", LedgerApiV2.TokenName "tk33")) 1
        !value_Tk34 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa7", LedgerApiV2.TokenName "tk34")) 1
        !value_Tk35 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa7", LedgerApiV2.TokenName "tk35")) 1
        !value_Tk36 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa8", LedgerApiV2.TokenName "tk36")) 1
        !value_Tk37 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa8", LedgerApiV2.TokenName "tk37")) 1
        !value_Tk38 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa8", LedgerApiV2.TokenName "tk38")) 1
        !value_Tk39 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa8", LedgerApiV2.TokenName "tk39")) 1
        !value_Tk40 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa8", LedgerApiV2.TokenName "tk40")) 1

        !testValuesList = [
            value_Tk33, value_Tk32, value_Tk31, value_Tk34, value_Tk35,
            value_Tk06, value_Tk07, value_Tk10, value_Tk09, value_Tk08,
            value_Tk16, value_Tk17, value_Tk18, value_Tk19, value_Tk20,
            value_Tk01, value_Tk03, value_Tk02, value_Tk04, value_Tk05,
            value_Tk21, value_Tk22, value_Tk23, value_Tk24, value_Tk25,
            value_Tk12, value_Tk11, value_Tk13, value_Tk15, value_Tk14,
            value_Tk28, value_Tk27, value_Tk26, value_Tk29, value_Tk30,
            value_Tk36, value_Tk40, value_Tk38, value_Tk39, value_Tk37
            ]

        !testValues =  [ foldl (<>) mempty (P.take i testValuesList) | i <- [0..40] ]

        testMintingPolicies :: LedgerValue.Value -> P.IO ()
        testMintingPolicies = evaluateCaseInMintingPolicy

    in do

        P.putStrLn "---------------"

        P.putStrLn"Testing Eq Methods:"

        P.putStrLn "-----"

        testEqMethods

        P.putStrLn "----------------"
        P.putStrLn "Case,Different Assets,Currency Symbols,Method,Mem,CPU,Size"

        mapM_ testMintingPolicies testValues


---------------------------------------------------

evaluateScriptMint :: LedgerApiV2.MintingPolicy -> [PlutusTx.Data] -> (P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, LedgerApiV2.LogOutput, Integer)
evaluateScriptMint p datas =
    let
        getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
        getScriptMintingPolicy    = LedgerApiV2.getMintingPolicy

        getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
        getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000

        !pv = LedgerProtocolVersionsV1.vasilPV
        !scriptMintingPolicyV2 = getScriptMintingPolicy p
        !scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        !(log, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas
        !size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
   in
        (e, log, size)

---------------------------------------------------

removeElements :: Integer -> [LedgerApiV2.Value] -> [LedgerApiV2.Value]
removeElements _ [] = []
removeElements s (x:xs)
    |s>0 = x : removeElements (s-1) xs
    |otherwise = xs

createValueFromCombiningValues :: Integer ->  [LedgerApiV2.Value] -> LedgerApiV2.Value
createValueFromCombiningValues _ [] = mempty
createValueFromCombiningValues factIndex list =
    let
        fact n' = if n' == 0 then 1 else n' * fact(n'-1)

        !len = length list
        !n = fact (len-1)

        !index = ((factIndex - 1) `divide` n) + 1
        !value = list!!(index-1)

        !list' = removeElements (index-1) list
        !factIndex' = factIndex - ((index-1)*n)
    in
        value <> createValueFromCombiningValues factIndex' list'

checkEqMethod :: (LedgerApiV2.Value -> LedgerApiV2.Value -> Bool) -> [LedgerApiV2.Value] -> LedgerApiV2.Value -> LedgerApiV2.Value -> P.IO Bool
checkEqMethod valueEqualsValue list1 valueZero1 valueZero2 =
    let
        fact n = if n == 0 then 1 else n * fact(n-1)

        cases1 = [1..fact (length list1)]
        values1 = [ createValueFromCombiningValues i list1  | i <- cases1]

        list2 = (valueZero1:list1)
        cases2 = [1..fact (length list2)]
        values2 = [ createValueFromCombiningValues i list2  | i <- cases2]

        list3 = (valueZero2:list1)
        cases3 = [1..fact (length list3)]
        values3 = [ createValueFromCombiningValues i list3  | i <- cases3]

        list4 = (valueZero1:valueZero2:list1)
        cases4 = [1..fact (length list4)]
        values4 = [ createValueFromCombiningValues i list4 | i <- cases4]

        values = values1 ++ values2 ++ values3 ++ values4
    in do
        -- P.putStrLn $ "Comparing: " ++ P.show (length values) ++ " values"
        return $ all (\value1 -> all (valueEqualsValue value1) values ) values

---------------------------------------------------

testEqMethods :: P.IO ()
testEqMethods =
    let
        -- value1: a new currency symbol
        !value1 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value2: existing currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk01")) 1

        -- value3: a new currency symbol
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value2) and same name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1

        -- zero in same currency symbol but different name
        !valueZero1 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk02")) 0

        -- zero in a new currency symbol 
        !valueZero2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2", LedgerApiV2.TokenName "tk02")) 0

        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        -- For testing the operators in all possible combinations of mixing the values
        -- all of them are the same, but creates in different order
        -- only operators that can handle des-normalized values van pass this tests 

        !list = [value1, value2, value3, value4, valueADA]

    in do
        testEqualitiesUnsafe <- checkEqMethod normalizedValueEqualsNormalizedValue list valueZero1 valueZero2
        testEqualities1 <- checkEqMethod valueEqualsValue1 list valueZero1 valueZero2
        testEqualities1'Sorted <- checkEqMethod valueEqualsValue1'Sorted list valueZero1 valueZero2
        testEqualities2 <- checkEqMethod valueEqualsValue2 list valueZero1 valueZero2
        testEqualities3 <- checkEqMethod valueEqualsValue3 list valueZero1 valueZero2
        testEqualities3'Sorted <- checkEqMethod valueEqualsValue3'Sorted list valueZero1 valueZero2
        testEqualities4 <- checkEqMethod valueEqualsValue4 list valueZero1 valueZero2
        testEqualities4'Sorted <- checkEqMethod valueEqualsValue4'Sorted list valueZero1 valueZero2
        testEqualities4'SmartSorted <- checkEqMethod valueEqualsValue4'SmartSorted list valueZero1 valueZero2
        testEqualities5 <- checkEqMethod valueEqualsValue5 list valueZero1 valueZero2
        testEqualities6 <- checkEqMethod valueEqualsValue6 list valueZero1 valueZero2
        testEqualities7 <- checkEqMethod valueEqualsValue7 list valueZero1 valueZero2

        P.putStrLn"Testing Method Unsafe..."
        if testEqualitiesUnsafe then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 1..."
        if testEqualities1 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 1'Sorted..."
        if testEqualities1'Sorted then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 2..."
        if testEqualities2 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 3..."
        if testEqualities3 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 3'Sorted..."
        if testEqualities3'Sorted then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 4..."
        if testEqualities4 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 4'Sorted..."
        if testEqualities4'Sorted then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 4'SmartSorted..."
        if testEqualities4'SmartSorted then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 5..."
        if testEqualities5 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 6..."
        if testEqualities6 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

        P.putStrLn"Testing Method 7..."
        if testEqualities7 then
            P.putStrLn "OK"
        else
            P.putStrLn "Failed"

---------------------------------------------------

evaluateCaseInMintingPolicy :: LedgerValue.Value -> P.IO ()
evaluateCaseInMintingPolicy v =
    let
        !curSymbol1 = curSymbol policy1
        !curSymbol1'Sorted = curSymbol policy1'Sorted
        !curSymbol2 = curSymbol policy2
        !curSymbol3 = curSymbol policy3
        !curSymbol3'Sorted = curSymbol policy3'Sorted
        !curSymbol4 = curSymbol policy4
        !curSymbol4'Sorted = curSymbol policy4'Sorted
        !curSymbol4'SmartSorted = curSymbol policy4'SmartSorted
        !curSymbol5 = curSymbol policy5
        !curSymbol6 = curSymbol policy6
        !curSymbol7 = curSymbol policy7

        exampleTxOutRef :: LedgerApiV2.TxOutRef
        exampleTxOutRef = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aaccff",
            LedgerApiV2.txOutRefIdx = 10
        }

        exampleAddress :: LedgerAddress.Address
        exampleAddress =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" , LedgerApiV2.addressStakingCredential =  Nothing }

        caseValue = length $ LedgerValue.flattenValue v

        !value1 = v
        -- value2: a new currency symbol and a new name
        !value2 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value3: existing currency symbol (in value2) and same name
        !value3 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", LedgerApiV2.TokenName "tk00")) 1
        -- value4: existing currency symbol (in value1) and a new name
        !value4 = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1", LedgerApiV2.TokenName "tk00")) 1
        -- value for ADA
        !valueADA = LedgerAda.lovelaceValueOf 433

        -- Output
        !value_Real = value1 <> value2 <> value3 <> value4 <> valueADA

        --------------------------------

        mockInput1 :: LedgerApiV2.TxInInfo
        mockInput1 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress
                    value1
                    LedgerApiV2.NoOutputDatum
                    DataMaybe.Nothing
                )

        mockOutput1 :: LedgerApiV2.TxOut
        mockOutput1 =
            LedgerApiV2.TxOut
                exampleAddress
                value_Real
                LedgerApiV2.NoOutputDatum
                DataMaybe.Nothing

        --------------------------------

        redeemer_For_Mint = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ()

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInput1 ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutput1 ]

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = LedgerAda.lovelaceValueOf 0

        mockScriptPurposeMint1 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint1 = LedgerApiV2.Minting curSymbol1

        mockScriptPurposeMint1'Sorted :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint1'Sorted = LedgerApiV2.Minting curSymbol1'Sorted

        mockScriptPurposeMint2 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint2 = LedgerApiV2.Minting curSymbol2

        mockScriptPurposeMint3 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint3 = LedgerApiV2.Minting curSymbol3

        mockScriptPurposeMint3'Sorted :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint3'Sorted = LedgerApiV2.Minting curSymbol3'Sorted

        mockScriptPurposeMint4 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint4 = LedgerApiV2.Minting curSymbol4

        mockScriptPurposeMint4'Sorted :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint4'Sorted = LedgerApiV2.Minting curSymbol4'Sorted

        mockScriptPurposeMint4'SmartSorted :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint4'SmartSorted = LedgerApiV2.Minting curSymbol4'SmartSorted

        mockScriptPurposeMint5 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint5 = LedgerApiV2.Minting curSymbol5

        mockScriptPurposeMint6 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint6 = LedgerApiV2.Minting curSymbol6

        mockScriptPurposeMint7 :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeMint7 = LedgerApiV2.Minting curSymbol7

        mockRedeemerMint1 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint1 = (mockScriptPurposeMint1, redeemer_For_Mint)

        mockRedeemerMint1'Sorted :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint1'Sorted = (mockScriptPurposeMint1'Sorted, redeemer_For_Mint)

        mockRedeemerMint2 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint2 = (mockScriptPurposeMint2, redeemer_For_Mint)

        mockRedeemerMint3'Sorted :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint3'Sorted = (mockScriptPurposeMint3'Sorted, redeemer_For_Mint)

        mockRedeemerMint3 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint3 = (mockScriptPurposeMint3, redeemer_For_Mint)

        mockRedeemerMint4 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint4 = (mockScriptPurposeMint4, redeemer_For_Mint)

        mockRedeemerMint4'Sorted :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint4'Sorted = (mockScriptPurposeMint4'Sorted, redeemer_For_Mint)

        mockRedeemerMint4'SmartSorted :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint4'SmartSorted = (mockScriptPurposeMint4'SmartSorted, redeemer_For_Mint)

        mockRedeemerMint5 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint5 = (mockScriptPurposeMint5, redeemer_For_Mint)

        mockRedeemerMint6 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint6 = (mockScriptPurposeMint6, redeemer_For_Mint)

        mockRedeemerMint7 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint7 = (mockScriptPurposeMint7, redeemer_For_Mint)

        mockTxInfoRedeemers1 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers1 = LedgerApiV2.fromList [ mockRedeemerMint1 ]

        mockTxInfoRedeemers1'Sorted :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers1'Sorted = LedgerApiV2.fromList [ mockRedeemerMint1'Sorted ]

        mockTxInfoRedeemers2 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers2 = LedgerApiV2.fromList [ mockRedeemerMint2 ]

        mockTxInfoRedeemers3 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers3 = LedgerApiV2.fromList [ mockRedeemerMint3 ]

        mockTxInfoRedeemers3'Sorted  :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers3'Sorted  = LedgerApiV2.fromList [ mockRedeemerMint3'Sorted  ]

        mockTxInfoRedeemers4 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers4 = LedgerApiV2.fromList [ mockRedeemerMint4 ]

        mockTxInfoRedeemers4'Sorted  :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers4'Sorted  = LedgerApiV2.fromList [ mockRedeemerMint4'Sorted  ]

        mockTxInfoRedeemers4'SmartSorted  :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers4'SmartSorted  = LedgerApiV2.fromList [ mockRedeemerMint4'SmartSorted ]

        mockTxInfoRedeemers5 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers5 = LedgerApiV2.fromList [ mockRedeemerMint5 ]

        mockTxInfoRedeemers6 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers6 = LedgerApiV2.fromList [ mockRedeemerMint6 ]

        mockTxInfoRedeemers7 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers7 = LedgerApiV2.fromList [ mockRedeemerMint7 ]

        !now = LedgerApiV2.POSIXTime 1000000
        !intervalOffset1 = 1000
        !intervalOffset2 = 15000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        mockCtx1 :: LedgerApiV2.ScriptContext
        mockCtx1 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers1
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint1

        mockCtx1'Sorted :: LedgerApiV2.ScriptContext
        mockCtx1'Sorted = LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers1'Sorted
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint1'Sorted

        mockCtx2 :: LedgerApiV2.ScriptContext
        mockCtx2 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers2
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint2

        mockCtx3 :: LedgerApiV2.ScriptContext
        mockCtx3 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers3
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint3

        mockCtx3'Sorted :: LedgerApiV2.ScriptContext
        mockCtx3'Sorted =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers3'Sorted
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint3'Sorted

        mockCtx4 :: LedgerApiV2.ScriptContext
        mockCtx4 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers4
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint4

        mockCtx4'Sorted :: LedgerApiV2.ScriptContext
        mockCtx4'Sorted =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers4'Sorted
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint4'Sorted

        mockCtx4'SmartSorted :: LedgerApiV2.ScriptContext
        mockCtx4'SmartSorted =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers4'SmartSorted
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint4'SmartSorted

        mockCtx5 :: LedgerApiV2.ScriptContext
        mockCtx5 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers5
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint5

        mockCtx6 :: LedgerApiV2.ScriptContext
        mockCtx6 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers6
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint6

        mockCtx7 :: LedgerApiV2.ScriptContext
        mockCtx7 =
            LedgerApiV2.ScriptContext
                (
                    LedgerApiV2.TxInfo
                    mockTxInfoInputs
                    [ ]
                    mockTxInfoOutputs
                    (LedgerAda.lovelaceValueOf 50000)
                    mockTxInfoMint
                    []
                    (LedgerApiV2.fromList [])
                    validityRange
                    []
                    mockTxInfoRedeemers7
                    (LedgerApiV2.fromList [])
                    (LedgerApiV2.TxId "555")
                )
                mockScriptPurposeMint7

        !datas1  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx1]
        !datas1'Sorted = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx1'Sorted]
        !datas2  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx2]
        !datas3  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx3]
        !datas3'Sorted  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx3'Sorted]
        !datas4  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx4]
        !datas4'Sorted  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx4'Sorted]
        !datas4'SmartSorted  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx4'SmartSorted]
        !datas5  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx5]
        !datas6  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx6]
        !datas7  = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx7]

        --------------------------------

        (e1, log1, size1) = evaluateScriptMint policy1 datas1
        (e1'Sorted, log1'Sorted, size1'Sorted) = evaluateScriptMint policy1'Sorted datas1'Sorted
        (e2, log2, size2) = evaluateScriptMint policy2 datas2
        (e3, log3, size3) = evaluateScriptMint policy3 datas3
        (e3'Sorted, log3'Sorted, size3'Sorted) = evaluateScriptMint policy3'Sorted datas3'Sorted
        (e4, log4, size4) = evaluateScriptMint policy4 datas4
        (e4'Sorted, log4'Sorted, size4'Sorted) = evaluateScriptMint policy4'Sorted datas4'Sorted
        (e4'SmartSorted, log4'SmartSorted, size4'SmartSorted) = evaluateScriptMint policy4'SmartSorted datas4'SmartSorted
        (e5, log5, size5) = evaluateScriptMint policy5 datas5
        (e6, log6, size6) = evaluateScriptMint policy6 datas6
        (e7, log7, size7) = evaluateScriptMint policy7 datas7

        (e1_plutonomy, log1_plutonomy, size1_plutonomy) = evaluateScriptMint policy1_fromPlutonomy datas1
        (e1'Sorted_plutonomy, log1'Sorted_plutonomy, size1'Sorted_plutonomy) = evaluateScriptMint policy1'Sorted_fromPlutonomy datas1'Sorted
        (e2_plutonomy, log2_plutonomy, size2_plutonomy) = evaluateScriptMint policy2_fromPlutonomy datas2
        (e3_plutonomy, log3_plutonomy, size3_plutonomy) = evaluateScriptMint policy3_fromPlutonomy datas3
        (e3'Sorted_plutonomy, log3'Sorted_plutonomy, size3'Sorted_plutonomy) = evaluateScriptMint policy3'Sorted_fromPlutonomy datas3'Sorted
        (e4_plutonomy, log4_plutonomy, size4_plutonomy) = evaluateScriptMint policy4_fromPlutonomy datas4
        (e4'Sorted_plutonomy, log4'Sorted_plutonomy, size4'Sorted_plutonomy) = evaluateScriptMint policy4'Sorted_fromPlutonomy datas4'Sorted
        (e4'SmartSorted_plutonomy, log4'SmartSorted_plutonomy, size4'SmartSorted_plutonomy) = evaluateScriptMint policy4'SmartSorted_fromPlutonomy datas4
        (e5_plutonomy, log5_plutonomy, size5_plutonomy) = evaluateScriptMint policy5_fromPlutonomy datas5
        (e6_plutonomy, log6_plutonomy, size6_plutonomy) = evaluateScriptMint policy6_fromPlutonomy datas5
        (e7_plutonomy, log7_plutonomy, size7_plutonomy) = evaluateScriptMint policy7_fromPlutonomy datas5

        --------------------------------

    in do

        -- Exporting as table in CSV:

        case e1 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1, " ++ "Eval Error, ," ++  P.show size1
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size1

        case e1'Sorted of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1'Sorted, " ++ "Eval Error, ," ++  P.show size1'Sorted
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size1'Sorted

        case e2 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue2, " ++ "Eval Error, ," ++  P.show size2
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue2, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size2

        case e3 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3, " ++ "Eval Error, ," ++  P.show size3
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size3

        case e3'Sorted of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3'Sorted, " ++ "Eval Error, ," ++  P.show size3'Sorted
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size3'Sorted

        case e4 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4, " ++ "Eval Error, ," ++  P.show size4
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4

        case e4'Sorted of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'Sorted, " ++ "Eval Error, ," ++  P.show size4'Sorted
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4'Sorted

        case e4'SmartSorted of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'SmartSorted, " ++ "Eval Error, ," ++  P.show size4'SmartSorted
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'SmartSorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4'SmartSorted

        case e5 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue5, " ++ "Eval Error, ," ++  P.show size5
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue5, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size5

        case e6 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue6, " ++ "Eval Error, ," ++  P.show size6
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue6, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size6

        case e7 of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue7, " ++ "Eval Error, ," ++  P.show size7
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue7, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size7

        case e1_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1, " ++ "Eval Error, ," ++  P.show size1_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size1_plutonomy

        case e1'Sorted_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1'Sorted, " ++ "Eval Error, ," ++  P.show size1'Sorted_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue1'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size1'Sorted_plutonomy

        case e2_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue2, " ++ "Eval Error, ," ++  P.show size2_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue2, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size2_plutonomy

        case e3_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3, " ++ "Eval Error, ," ++  P.show size3_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size3_plutonomy

        case e3'Sorted_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3'Sorted, " ++ "Eval Error, ," ++  P.show size3'Sorted_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue3'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size3'Sorted_plutonomy

        case e4_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4, " ++ "Eval Error, ," ++  P.show size4_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4_plutonomy

        case e4'Sorted_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'Sorted, " ++ "Eval Error, ," ++  P.show size4'Sorted_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'Sorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4'Sorted_plutonomy

        case e4'SmartSorted_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'SmartSorted, " ++ "Eval Error, ," ++  P.show size4'SmartSorted_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue4'SmartSorted, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size4'SmartSorted_plutonomy

        case e5_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue5, " ++ "Eval Error, ," ++  P.show size5_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue5, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size5_plutonomy

        case e6_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue6, " ++ "Eval Error, ," ++  P.show size6_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue6, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size6_plutonomy

        case e7_plutonomy of
            Left _ -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue7, " ++ "Eval Error, ," ++  P.show size7_plutonomy
            Right exbudget -> do
                P.putStrLn $ P.show caseValue ++ " (Plutonomy)" ++ ", " ++ P.show caseValue ++ ", " ++ P.show ( (caseValue-1) `divide` 5 + 1) ++ ", valueEqualsValue7, " ++ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++  P.show size7_plutonomy


--------------------------------------------------------------------------------
