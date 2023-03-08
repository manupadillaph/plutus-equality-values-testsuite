Using serializeData alone is the best in terms of resources, but it will not work in all cases. Even creating values on-chain using the (<>) operator will create values that are not normalized.

I wrote several methods of equality checks, all of which are safe, meaning that they can work with normalized or des-normalized values.

I checked all these methods against all kinds of value variations in different orders. I wrote a method to test them (testEqMethods) in thousands of different cases.

I create the same value combining values in all the possible orders, from same currency symbol and different, with some amount or sometimes with zero amount and with ADA value. In specific there were 6600 combinations to create the same value, and I compare each one of them against the 6600 combinations again.

For example, to create a value using four values: value1, value2, value3, value4 there will be 24 (factorial of 4) combinations. 
```
value = value1 <> value2 <> value3 <> value4 
...
value = value2 <> value3 <> value4 <> value1 
...
value = value4 <> value2 <> value3 <> value1 
...
```
In the results of  this test you can see:

```
Testing Eq Methods:
-----
Testing Method Unsafe...
Failed
Testing 1...
OK
Testing 1...
OK
Testing 2...
OK
Testing 3...
OK
Testing 3'Sorted...
OK
Testing 4...
OK
Testing 4'Sorted...
OK
Testing 4'SmartSorted...
OK
Testing 5...
OK
Testing 6...
OK
Testing 7...
OK
```

The first method in that list was using serializeData, and as you can see, it failed when comparing values that were not normalized. However, the other methods did not fail.

Then I evaluated all of them in 40 different cases on-chain. Each case is creating a value with an increment on one NFT up to 40. The assets come from 8 different collections (different currency symbols), with 5 in each collection.

So cases 1 to 5 are using values with 1 to 5 NFT from the same currency symbol. Cases 6 to 10 are from a second collection and so on.

These are the methods I tested: 

1.  valueEqualsValue1: It uses flattened values without zeros and removes elements from the lists when it finds them. Its one of the best I tested.
```
{-# INLINABLE valueEqualsValue1 #-}
valueEqualsValue1 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue1 !value1 !value2 =
    let
        !flattenedValue1 = flattenValueAndDeleteZeros value1
        !flattenedValue2 = flattenValueAndDeleteZeros value2
    in
        flattenedValue1 `flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros` flattenedValue2

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
```

2. valueEqualsValue1'Sorted: It is a variation of method 1. It sorts the flattened values before iterating on them.
```
{-# INLINABLE valueEqualsValue1'Sorted #-}
valueEqualsValue1'Sorted :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue1'Sorted !value1 !value2 =
    let
        !flattenedValue1 = flattenAndSortValueAndDeleteZeros value1
        !flattenedValue2 = flattenAndSortValueAndDeleteZeros value2
    in
        flattenedValue1 `flattenedValueWithoutZerosEqualsFlattenedValueWithoutZeros` flattenedValue2
```

3. valueEqualsValue2: It uses flattened values without zeros to compare lengths, and then uses assetClassValueOf to check for the assets. This is one of the bests method, competing with valueEqualsValue1.
```
{-# INLINABLE valueEqualsValue2 #-}
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

```

4. valueEqualsValue3: It's a subtle variation of valueEqualsValue2 that uses flattened values without zeros to compare lengths. Then it uses my own version of assetClassValueOf to check for the assets. It uses the maps directly from the beginning without the need to recreate the AssetClass that is required to use the assetClassValueOf function. However, I can't explain why this method gave me worse results than the original one, which is going back and forth between values, maps, and AssetClasses.
```
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

```

5. valueEqualsValue3'Sorted: It is a variation of valueEqualsValue3 that sorts the flattened values before searching.
```
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

```


6. valueEqualsValue4: It compares the currency symbols and then the lists inside each of them. It removes the elements, currency symbols, and token names from the lists in the process. This is the method which gave me the best results in all the cases I tested.
```
{-# INLINABLE valueEqualsValue4 #-}
valueEqualsValue4 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue4 (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        !listCS1 = TxAssocMap.toList mp1
        !listCS2 = TxAssocMap.toList mp2
    in
        listCS1 `listCSEqualsListCS` listCS2 

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

```

7. valueEqualsValue4'Sorted: It is a variation of valueEqualsValue4 that sorts the currency symbols list and token names before searching.
```

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
```

8. valueEqualsValue4'SmartSorted: It is a variation of valueEqualsValue4'Sorted that only sorts when the length of the lists exceeds a certain number. I tried different numbers, but I didn't see any improvement. Just the length calculation was already overloading the method too much. In all the tests I did, the simple valueEqualsValue4 method performed better, even when the lists are disordered. Perhaps there is an improvement in really large lists, but this is not a reality in on-chain real values. We will need to think about the most common scenarios where we will be using these operations.
```
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
```

9. valueEqualsValue5: It flattens and sorts the values first and then compares them with serializeData.
```
valueEqualsValue5 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue5 !value1 !value2 =
   let
        !sortedFlattenedValue1 = flattenAndSortValueAndDeleteZeros value1
        !sortedFlattenedValue2 = flattenAndSortValueAndDeleteZeros value2
    in
        sortedFlattenedValue1 `sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros` sortedFlattenedValue2

---------------------------------------------------

{-# INLINABLE sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros #-}
sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
sortedAndFlattenedValueWithoutZerosEqualsSortedAndFlattenedValueWithoutZeros !sortedFlattenValue1 !sortedFlattenValue2 =
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData sortedFlattenValue1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData sortedFlattenValue2)

```

10. valueEqualsValue6: It normalizes the values first and then compares them with serializeData. For normlize the value is using a copy of the internal built-in method.
```
{-# INLINABLE valueEqualsValue6 #-}
valueEqualsValue6 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue6 !value1 !value2 =
    let
        !normalizedValue1 = normalizeValue value1
        !normalizedValue2 = normalizeValue value2
    in
        normalizedValue1 `normalizedValueEqualsNormalizedValue` normalizedValue2

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

{-# INLINABLE normalizedValueEqualsNormalizedValue #-}
normalizedValueEqualsNormalizedValue ::LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
normalizedValueEqualsNormalizedValue !normalizedValue1 !normalizedValue2 =
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData normalizedValue1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData normalizedValue2)
```

11. valueEqualsValue7: It uses the built-in operator (==) just for comparison with the others.

```
{-# INLINABLE valueEqualsValue7 #-}
valueEqualsValue7 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue7 !value1 !value2 =
    value1 == value2
```

And this are all the results:

| Case           | Different Assets | Currency Symbols | Method                         | Mem         | CPU           | Size  |
|----------------|------------------|------------------|--------------------------------|-------------|---------------|-------|
| 0              | 0                | 0                |  valueEqualsValue4             | 1,361,282   | 359,457,523   | 4,143 |
| 0              | 0                | 0                |  valueEqualsValue1             | 1,434,182   | 376,224,523   | 3,901 |
| 0              | 0                | 0                |  valueEqualsValue2             | 1,512,898   | 396,151,637   | 3,824 |
| 0              | 0                | 0                |  valueEqualsValue3             | 1,519,398   | 397,646,637   | 3,849 |
| 0              | 0                | 0                |  valueEqualsValue4'SmartSorted | 1,681,034   | 439,231,161   | 5,198 |
| 0              | 0                | 0                |  valueEqualsValue4'Sorted      | 1,694,394   | 437,779,349   | 4,888 |
| 0              | 0                | 0                |  valueEqualsValue7             | 1,725,896   | 445,559,192   | 3,515 |
| 0              | 0                | 0                |  valueEqualsValue5             | 1,747,126   | 509,121,643   | 4,332 |
| 0              | 0                | 0                |  valueEqualsValue1'Sorted      | 1,757,294   | 452,246,349   | 4,529 |
| 0              | 0                | 0                |  valueEqualsValue3'Sorted      | 1,885,414   | 484,186,575   | 4,495 |
| 0              | 0                | 0                |  valueEqualsValue6             | 2,025,742   | 575,522,651   | 5,409 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue4             | 1,108,018   | 293,898,407   | 3,194 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue1             | 1,169,118   | 307,951,407   | 2,975 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue4'SmartSorted | 1,222,638   | 322,639,081   | 4,003 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue2             | 1,223,134   | 322,197,521   | 2,877 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue3             | 1,233,634   | 324,612,521   | 2,900 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue4'Sorted      | 1,407,130   | 364,400,233   | 3,793 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue5             | 1,412,162   | 424,771,527   | 3,305 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue7             | 1,412,162   | 424,771,527   | 3,305 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue1'Sorted      | 1,457,430   | 375,969,233   | 3,531 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue3'Sorted      | 1,527,650   | 394,592,459   | 3,460 |
| 0 (Plutonomy)  | 0                | 0                |  valueEqualsValue6             | 1,640,178   | 479,534,535   | 4,248 |
| 1              | 1                | 1                |  valueEqualsValue4             | 1,768,704   | 460,178,069   | 4,143 |
| 1              | 1                | 1                |  valueEqualsValue1             | 1,864,404   | 482,189,069   | 3,901 |
| 1              | 1                | 1                |  valueEqualsValue2             | 1,972,928   | 509,978,663   | 3,824 |
| 1              | 1                | 1                |  valueEqualsValue3             | 1,981,528   | 511,956,663   | 3,849 |
| 1              | 1                | 1                |  valueEqualsValue4'Sorted      | 2,266,842   | 580,213,945   | 4,888 |
| 1              | 1                | 1                |  valueEqualsValue7             | 2,307,634   | 588,822,604   | 3,515 |
| 1              | 1                | 1                |  valueEqualsValue5             | 2,336,844   | 674,876,934   | 4,332 |
| 1              | 1                | 1                |  valueEqualsValue1'Sorted      | 2,348,942   | 599,096,945   | 4,529 |
| 1              | 1                | 1                |  valueEqualsValue3'Sorted      | 2,529,872   | 644,515,207   | 4,495 |
| 1              | 1                | 1                |  valueEqualsValue4'SmartSorted | 2,738,354   | 703,218,371   | 5,198 |
| 1              | 1                | 1                |  valueEqualsValue6             | 2,760,688   | 778,747,012   | 5,409 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue4             | 1,483,512   | 387,131,957   | 3,194 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue1             | 1,563,012   | 405,416,957   | 2,975 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue2             | 1,634,036   | 424,581,551   | 2,877 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue3             | 1,648,336   | 427,870,551   | 2,900 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue4'SmartSorted | 1,849,076   | 479,751,414   | 4,003 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue4'Sorted      | 1,929,950   | 495,276,833   | 3,793 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue5             | 1,939,952   | 576,139,822   | 3,305 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue7             | 1,939,952   | 576,139,822   | 3,305 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue1'Sorted      | 1,995,050   | 510,249,833   | 3,531 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue3'Sorted      | 2,096,880   | 537,475,095   | 3,460 |
| 1 (Plutonomy)  | 1                | 1                |  valueEqualsValue6             | 2,291,196   | 663,311,900   | 4,248 |
| 2              | 2                | 1                |  valueEqualsValue4             | 1,872,132   | 486,924,791   | 4,143 |
| 2              | 2                | 1                |  valueEqualsValue1             | 2,033,140   | 525,173,264   | 3,901 |
| 2              | 2                | 1                |  valueEqualsValue2             | 2,125,964   | 549,141,493   | 3,824 |
| 2              | 2                | 1                |  valueEqualsValue3             | 2,136,264   | 551,510,493   | 3,849 |
| 2              | 2                | 1                |  valueEqualsValue4'Sorted      | 2,457,678   | 628,213,163   | 4,888 |
| 2              | 2                | 1                |  valueEqualsValue7             | 2,478,866   | 631,773,912   | 3,515 |
| 2              | 2                | 1                |  valueEqualsValue5             | 2,542,252   | 745,873,672   | 4,332 |
| 2              | 2                | 1                |  valueEqualsValue1'Sorted      | 2,555,280   | 650,958,926   | 4,529 |
| 2              | 2                | 1                |  valueEqualsValue3'Sorted      | 2,772,016   | 705,321,533   | 4,495 |
| 2              | 2                | 1                |  valueEqualsValue4'SmartSorted | 2,871,188   | 737,346,524   | 5,198 |
| 2              | 2                | 1                |  valueEqualsValue6             | 2,986,524   | 843,432,254   | 5,409 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue4             | 1,571,976   | 410,365,181   | 3,194 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue1             | 1,707,384   | 442,725,654   | 2,975 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue2             | 1,764,008   | 458,367,883   | 2,877 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue3             | 1,781,708   | 462,438,883   | 2,900 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue4'SmartSorted | 1,947,042   | 505,376,115   | 4,003 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue4'Sorted      | 2,101,522   | 538,773,553   | 3,793 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue5             | 2,119,496   | 641,116,062   | 3,305 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue7             | 2,119,496   | 641,116,062   | 3,305 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue1'Sorted      | 2,179,724   | 557,057,316   | 3,531 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue3'Sorted      | 2,309,860   | 591,501,923   | 3,460 |
| 2 (Plutonomy)  | 2                | 1                |  valueEqualsValue6             | 2,489,568   | 721,608,644   | 4,248 |
| 3              | 3                | 1                |  valueEqualsValue4             | 1,975,560   | 513,671,513   | 4,143 |
| 3              | 3                | 1                |  valueEqualsValue1             | 2,197,976   | 567,260,459   | 3,901 |
| 3              | 3                | 1                |  valueEqualsValue2             | 2,284,802   | 589,935,714   | 3,824 |
| 3              | 3                | 1                |  valueEqualsValue3             | 2,297,002   | 592,741,714   | 3,849 |
| 3              | 3                | 1                |  valueEqualsValue4'Sorted      | 2,596,114   | 664,160,381   | 4,888 |
| 3              | 3                | 1                |  valueEqualsValue7             | 2,679,904   | 682,471,393   | 3,515 |
| 3              | 3                | 1                |  valueEqualsValue5             | 2,695,260   | 804,818,348   | 4,332 |
| 3              | 3                | 1                |  valueEqualsValue1'Sorted      | 2,709,218   | 690,768,907   | 4,529 |
| 3              | 3                | 1                |  valueEqualsValue3'Sorted      | 2,967,762   | 755,753,250   | 4,495 |
| 3              | 3                | 1                |  valueEqualsValue4'SmartSorted | 3,004,022   | 771,474,677   | 5,198 |
| 3              | 3                | 1                |  valueEqualsValue6             | 3,159,960   | 896,065,558   | 5,409 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue4             | 1,660,440   | 433,598,405   | 3,194 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue1             | 1,848,256   | 479,229,351   | 2,975 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue2             | 1,898,982   | 493,601,606   | 2,877 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue3             | 1,920,282   | 498,500,606   | 2,900 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue4'SmartSorted | 2,045,008   | 531,000,816   | 4,003 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue4'Sorted      | 2,222,094   | 570,540,273   | 3,793 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue5             | 2,249,440   | 694,684,240   | 3,305 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue7             | 2,249,440   | 694,684,240   | 3,305 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue1'Sorted      | 2,313,398   | 592,134,799   | 3,531 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue3'Sorted      | 2,478,442   | 635,614,142   | 3,460 |
| 3 (Plutonomy)  | 3                | 1                |  valueEqualsValue6             | 2,638,340   | 768,497,450   | 4,248 |
| 4              | 4                | 1                |  valueEqualsValue4             | 2,078,988   | 540,418,235   | 4,143 |
| 4              | 4                | 1                |  valueEqualsValue1             | 2,362,812   | 609,347,654   | 3,901 |
| 4              | 4                | 1                |  valueEqualsValue2             | 2,449,442   | 632,361,326   | 3,824 |
| 4              | 4                | 1                |  valueEqualsValue3             | 2,463,742   | 635,650,326   | 3,849 |
| 4              | 4                | 1                |  valueEqualsValue4'Sorted      | 2,804,174   | 719,567,087   | 4,888 |
| 4              | 4                | 1                |  valueEqualsValue7             | 2,910,748   | 740,915,047   | 3,515 |
| 4              | 4                | 1                |  valueEqualsValue5             | 2,917,892   | 883,222,574   | 4,332 |
| 4              | 4                | 1                |  valueEqualsValue1'Sorted      | 2,932,780   | 750,038,376   | 4,529 |
| 4              | 4                | 1                |  valueEqualsValue3'Sorted      | 3,239,134   | 827,321,846   | 4,495 |
| 4              | 4                | 1                |  valueEqualsValue6             | 3,483,444   | 990,101,838   | 5,409 |
| 4              | 4                | 1                |  valueEqualsValue4'SmartSorted | 4,002,800   | 1,025,445,758 | 5,198 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue4             | 1,748,904   | 456,831,629   | 3,194 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue1             | 1,989,128   | 515,733,048   | 2,975 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue2             | 2,038,958   | 530,282,720   | 2,877 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue3             | 2,064,058   | 536,055,720   | 2,900 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue4'SmartSorted | 2,400,222   | 622,684,493   | 4,003 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue4'Sorted      | 2,402,690   | 619,558,481   | 3,793 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue5             | 2,440,808   | 765,825,968   | 3,305 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue7             | 2,440,808   | 765,825,968   | 3,305 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue1'Sorted      | 2,507,096   | 644,463,770   | 3,531 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue3'Sorted      | 2,713,650   | 698,793,240   | 3,460 |
| 4 (Plutonomy)  | 4                | 1                |  valueEqualsValue6             | 2,915,760   | 851,867,232   | 4,248 |
| 5              | 5                | 1                |  valueEqualsValue4             | 2,182,416   | 567,164,957   | 4,143 |
| 5              | 5                | 1                |  valueEqualsValue1             | 2,527,648   | 651,434,849   | 3,901 |
| 5              | 5                | 1                |  valueEqualsValue2             | 2,619,884   | 676,418,329   | 3,824 |
| 5              | 5                | 1                |  valueEqualsValue3             | 2,636,484   | 680,236,329   | 3,849 |
| 5              | 5                | 1                |  valueEqualsValue4'Sorted      | 2,999,210   | 768,532,305   | 4,888 |
| 5              | 5                | 1                |  valueEqualsValue5             | 3,127,500   | 955,185,250   | 4,332 |
| 5              | 5                | 1                |  valueEqualsValue1'Sorted      | 3,143,318   | 802,866,357   | 4,529 |
| 5              | 5                | 1                |  valueEqualsValue7             | 3,171,398   | 807,104,874   | 3,515 |
| 5              | 5                | 1                |  valueEqualsValue3'Sorted      | 3,503,484   | 894,126,345   | 4,495 |
| 5              | 5                | 1                |  valueEqualsValue6             | 3,713,480   | 1,055,753,142 | 5,409 |
| 5              | 5                | 1                |  valueEqualsValue4'SmartSorted | 4,410,458   | 1,126,229,399 | 5,198 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue4             | 1,837,368   | 480,064,853   | 3,194 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue1             | 2,130,000   | 552,236,745   | 2,975 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue2             | 2,183,936   | 568,411,225   | 2,877 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue3             | 2,213,036   | 575,104,225   | 2,900 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue4'Sorted      | 2,579,262   | 664,205,201   | 3,793 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue4'SmartSorted | 2,585,596   | 669,561,690   | 4,003 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue5             | 2,625,352   | 831,952,146   | 3,305 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue7             | 2,625,352   | 831,952,146   | 3,305 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue1'Sorted      | 2,696,770   | 692,421,253   | 3,531 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue3'Sorted      | 2,947,236   | 758,450,241   | 3,460 |
| 5 (Plutonomy)  | 5                | 1                |  valueEqualsValue6             | 3,119,132   | 911,314,038   | 4,248 |
| 6              | 6                | 2                |  valueEqualsValue4             | 2,613,040   | 673,519,266   | 4,143 |
| 6              | 6                | 2                |  valueEqualsValue1             | 2,979,772   | 762,734,158   | 3,901 |
| 6              | 6                | 2                |  valueEqualsValue2             | 3,085,716   | 791,877,118   | 3,824 |
| 6              | 6                | 2                |  valueEqualsValue3             | 3,104,616   | 796,224,118   | 3,849 |
| 6              | 6                | 2                |  valueEqualsValue4'Sorted      | 3,552,850   | 905,436,075   | 4,888 |
| 6              | 6                | 2                |  valueEqualsValue5             | 3,698,410   | 1,115,409,777 | 4,332 |
| 6              | 6                | 2                |  valueEqualsValue1'Sorted      | 3,716,158   | 944,186,127   | 4,529 |
| 6              | 6                | 2                |  valueEqualsValue7             | 3,782,942   | 958,115,575   | 3,515 |
| 6              | 6                | 2                |  valueEqualsValue3'Sorted      | 4,135,136   | 1,050,601,914 | 4,495 |
| 6              | 6                | 2                |  valueEqualsValue6             | 4,402,810   | 1,246,075,851 | 5,409 |
| 6              | 6                | 2                |  valueEqualsValue4'SmartSorted | 5,102,634   | 1,299,761,193 | 5,198 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue4             | 2,233,164   | 578,265,166   | 3,194 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue1             | 2,542,896   | 654,370,058   | 2,975 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue2             | 2,599,840   | 672,243,018   | 2,877 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue3             | 2,632,940   | 679,856,018   | 2,900 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue4'SmartSorted | 3,067,014   | 790,408,371   | 4,003 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue4'Sorted      | 3,085,874   | 790,148,975   | 3,793 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue5             | 3,135,534   | 978,065,677   | 3,305 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue7             | 3,135,534   | 978,065,677   | 3,305 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue1'Sorted      | 3,218,182   | 821,769,027   | 3,531 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue3'Sorted      | 3,504,060   | 897,571,814   | 3,460 |
| 6 (Plutonomy)  | 6                | 2                |  valueEqualsValue6             | 3,730,134   | 1,083,477,751 | 4,248 |
| 7              | 7                | 2                |  valueEqualsValue4             | 2,716,468   | 700,265,988   | 4,143 |
| 7              | 7                | 2                |  valueEqualsValue1             | 3,142,506   | 804,040,962   | 3,901 |
| 7              | 7                | 2                |  valueEqualsValue2             | 3,244,554   | 832,671,711   | 3,824 |
| 7              | 7                | 2                |  valueEqualsValue3             | 3,265,354   | 837,455,711   | 3,849 |
| 7              | 7                | 2                |  valueEqualsValue4'Sorted      | 3,745,086   | 953,757,293   | 4,888 |
| 7              | 7                | 2                |  valueEqualsValue5             | 3,905,218   | 1,186,728,453 | 4,332 |
| 7              | 7                | 2                |  valueEqualsValue1'Sorted      | 3,923,896   | 996,370,108   | 4,529 |
| 7              | 7                | 2                |  valueEqualsValue7             | 3,954,174   | 1,001,066,883 | 3,515 |
| 7              | 7                | 2                |  valueEqualsValue3'Sorted      | 4,384,682   | 1,113,408,003 | 4,495 |
| 7              | 7                | 2                |  valueEqualsValue6             | 4,630,046   | 1,311,083,155 | 5,409 |
| 7              | 7                | 2                |  valueEqualsValue4'SmartSorted | 5,235,468   | 1,333,889,346 | 5,198 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue4             | 2,321,628   | 601,498,390   | 3,194 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue1             | 2,682,466   | 690,277,364   | 2,975 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue2             | 2,734,814   | 707,477,113   | 2,877 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue3             | 2,771,514   | 715,918,113   | 2,900 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue4'SmartSorted | 3,164,980   | 816,033,072   | 4,003 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue4'Sorted      | 3,259,246   | 834,059,695   | 3,793 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue5             | 3,316,878   | 1,043,455,855 | 3,305 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue7             | 3,316,878   | 1,043,455,855 | 3,305 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue1'Sorted      | 3,404,656   | 868,990,510   | 3,531 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue3'Sorted      | 3,724,042   | 953,506,405   | 3,460 |
| 7 (Plutonomy)  | 7                | 2                |  valueEqualsValue6             | 3,930,306   | 1,142,188,557 | 4,248 |
| 8              | 8                | 2                |  valueEqualsValue4             | 2,819,896   | 727,012,710   | 4,143 |
| 8              | 8                | 2                |  valueEqualsValue1             | 3,305,240   | 845,347,766   | 3,901 |
| 8              | 8                | 2                |  valueEqualsValue2             | 3,409,194   | 875,097,695   | 3,824 |
| 8              | 8                | 2                |  valueEqualsValue3             | 3,432,094   | 880,364,695   | 3,849 |
| 8              | 8                | 2                |  valueEqualsValue4'Sorted      | 3,884,722   | 989,980,511   | 4,888 |
| 8              | 8                | 2                |  valueEqualsValue5             | 4,059,426   | 1,245,949,191 | 4,332 |
| 8              | 8                | 2                |  valueEqualsValue1'Sorted      | 4,079,034   | 1,036,456,089 | 4,529 |
| 8              | 8                | 2                |  valueEqualsValue7             | 4,155,212   | 1,051,764,364 | 3,515 |
| 8              | 8                | 2                |  valueEqualsValue3'Sorted      | 4,587,630   | 1,165,793,483 | 4,495 |
| 8              | 8                | 2                |  valueEqualsValue6             | 4,804,682   | 1,363,992,397 | 5,409 |
| 8              | 8                | 2                |  valueEqualsValue4'SmartSorted | 5,368,302   | 1,368,017,499 | 5,198 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue4             | 2,410,092   | 624,731,614   | 3,194 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue1             | 2,822,036   | 726,184,670   | 2,975 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue2             | 2,874,790   | 744,158,599   | 2,877 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue3             | 2,915,290   | 753,473,599   | 2,900 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue4'SmartSorted | 3,262,946   | 841,657,773   | 4,003 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue4'Sorted      | 3,381,018   | 866,102,415   | 3,793 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue5             | 3,448,022   | 1,097,300,095 | 3,305 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue7             | 3,448,022   | 1,097,300,095 | 3,305 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue1'Sorted      | 3,539,530   | 904,343,993   | 3,531 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue3'Sorted      | 3,899,026   | 999,388,387   | 3,460 |
| 8 (Plutonomy)  | 8                | 2                |  valueEqualsValue6             | 4,080,278   | 1,189,353,301 | 4,248 |
| 9              | 9                | 2                |  valueEqualsValue4             | 2,923,324   | 753,759,432   | 4,143 |
| 9              | 9                | 2                |  valueEqualsValue1             | 3,467,974   | 886,654,570   | 3,901 |
| 9              | 9                | 2                |  valueEqualsValue2             | 3,579,636   | 919,155,070   | 3,824 |
| 9              | 9                | 2                |  valueEqualsValue3             | 3,604,836   | 924,951,070   | 3,849 |
| 9              | 9                | 2                |  valueEqualsValue4'Sorted      | 4,096,382   | 1,046,215,217 | 4,888 |
| 9              | 9                | 2                |  valueEqualsValue5             | 4,285,658   | 1,325,181,355 | 4,332 |
| 9              | 9                | 2                |  valueEqualsValue1'Sorted      | 4,306,196   | 1,096,553,558 | 4,529 |
| 9              | 9                | 2                |  valueEqualsValue7             | 4,386,056   | 1,110,208,018 | 3,515 |
| 9              | 9                | 2                |  valueEqualsValue3'Sorted      | 4,868,604   | 1,239,867,842 | 4,495 |
| 9              | 9                | 2                |  valueEqualsValue6             | 5,131,766   | 1,458,856,677 | 5,409 |
| 9              | 9                | 2                |  valueEqualsValue4'SmartSorted | 6,385,680   | 1,626,266,580 | 5,198 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue4             | 2,498,556   | 647,964,838   | 3,194 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue1             | 2,961,606   | 762,091,976   | 2,975 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue2             | 3,019,768   | 782,287,476   | 2,877 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue3             | 3,064,268   | 792,522,476   | 2,900 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue4'Sorted      | 3,564,814   | 915,856,623   | 3,793 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue4'SmartSorted | 3,624,360   | 934,767,450   | 4,003 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue5             | 3,642,590   | 1,169,177,761 | 3,305 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue7             | 3,642,590   | 1,169,177,761 | 3,305 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue1'Sorted      | 3,736,428   | 957,408,964   | 3,531 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue3'Sorted      | 4,142,636   | 1,064,797,248 | 3,460 |
| 9 (Plutonomy)  | 9                | 2                |  valueEqualsValue6             | 4,360,898   | 1,273,459,083 | 4,248 |
| 10             | 10               | 2                |  valueEqualsValue4             | 3,026,752   | 780,506,154   | 4,143 |
| 10             | 10               | 2                |  valueEqualsValue1             | 3,630,708   | 927,961,374   | 3,901 |
| 10             | 10               | 2                |  valueEqualsValue2             | 3,755,880   | 964,843,836   | 3,824 |
| 10             | 10               | 2                |  valueEqualsValue3             | 3,783,580   | 971,214,836   | 3,849 |
| 10             | 10               | 2                |  valueEqualsValue4'Sorted      | 4,318,826   | 1,102,632,931 | 4,888 |
| 10             | 10               | 2                |  valueEqualsValue5             | 4,522,674   | 1,404,596,589 | 4,332 |
| 10             | 10               | 2                |  valueEqualsValue1'Sorted      | 4,544,142   | 1,156,834,035 | 4,529 |
| 10             | 10               | 2                |  valueEqualsValue7             | 4,646,706   | 1,176,397,845 | 3,515 |
| 10             | 10               | 2                |  valueEqualsValue3'Sorted      | 5,166,364   | 1,315,802,600 | 4,495 |
| 10             | 10               | 2                |  valueEqualsValue6             | 5,416,018   | 1,539,274,973 | 5,409 |
| 10             | 10               | 2                |  valueEqualsValue4'SmartSorted | 6,875,562   | 1,749,407,709 | 5,198 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue4             | 2,587,020   | 671,198,062   | 3,194 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue1             | 3,101,176   | 797,999,282   | 2,975 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue2             | 3,169,748   | 821,863,744   | 2,877 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue3             | 3,218,448   | 833,064,744   | 2,900 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue4'Sorted      | 3,764,794   | 967,035,839   | 3,793 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue4'SmartSorted | 3,833,142   | 988,177,143   | 4,003 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue5             | 3,850,542   | 1,241,836,497 | 3,305 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue7             | 3,850,542   | 1,241,836,497 | 3,305 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue1'Sorted      | 3,949,510   | 1,011,898,943 | 3,531 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue3'Sorted      | 4,404,832   | 1,132,480,508 | 3,460 |
| 10 (Plutonomy) | 10               | 2                |  valueEqualsValue6             | 4,610,086   | 1,345,740,881 | 4,248 |
| 11             | 11               | 3                |  valueEqualsValue4             | 3,453,476   | 885,963,463   | 4,143 |
| 11             | 11               | 3                |  valueEqualsValue1             | 4,082,832   | 1,039,260,683 | 3,901 |
| 11             | 11               | 3                |  valueEqualsValue2             | 4,227,514   | 1,081,934,388 | 3,824 |
| 11             | 11               | 3                |  valueEqualsValue3             | 4,257,714   | 1,088,880,388 | 3,849 |
| 11             | 11               | 3                |  valueEqualsValue4'Sorted      | 4,881,474   | 1,242,786,670 | 4,888 |
| 11             | 11               | 3                |  valueEqualsValue5             | 5,102,592   | 1,568,071,023 | 4,332 |
| 11             | 11               | 3                |  valueEqualsValue1'Sorted      | 5,125,990   | 1,301,403,774 | 4,529 |
| 11             | 11               | 3                |  valueEqualsValue7             | 5,288,056   | 1,335,155,835 | 3,515 |
| 11             | 11               | 3                |  valueEqualsValue3'Sorted      | 5,813,026   | 1,477,205,901 | 4,495 |
| 11             | 11               | 3                |  valueEqualsValue6             | 6,141,164   | 1,740,191,620 | 5,409 |
| 11             | 11               | 3                |  valueEqualsValue4'SmartSorted | 7,585,754   | 1,929,439,441 | 5,198 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue4             | 2,979,316   | 768,593,375   | 3,194 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue1             | 3,514,072   | 900,132,595   | 2,975 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue2             | 3,590,654   | 927,143,300   | 2,877 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue3             | 3,643,554   | 939,310,300   | 2,900 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue4'Sorted      | 4,277,814   | 1,095,631,582 | 3,793 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue4'SmartSorted | 4,320,968   | 1,111,675,793 | 4,003 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue5             | 4,368,532   | 1,390,923,935 | 3,305 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue7             | 4,368,532   | 1,390,923,935 | 3,305 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue1'Sorted      | 4,477,330   | 1,143,898,686 | 3,531 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue3'Sorted      | 4,974,666   | 1,276,069,813 | 3,460 |
| 11 (Plutonomy) | 11               | 3                |  valueEqualsValue6             | 5,251,304   | 1,527,210,532 | 4,248 |
| 12             | 12               | 3                |  valueEqualsValue4             | 3,556,904   | 912,710,185   | 4,143 |
| 12             | 12               | 3                |  valueEqualsValue1             | 4,245,566   | 1,080,567,487 | 3,901 |
| 12             | 12               | 3                |  valueEqualsValue2             | 4,392,154   | 1,124,360,744 | 3,824 |
| 12             | 12               | 3                |  valueEqualsValue3             | 4,424,454   | 1,131,789,744 | 3,849 |
| 12             | 12               | 3                |  valueEqualsValue4'Sorted      | 5,073,710   | 1,291,107,888 | 4,888 |
| 12             | 12               | 3                |  valueEqualsValue5             | 5,309,400   | 1,639,389,761 | 4,332 |
| 12             | 12               | 3                |  valueEqualsValue1'Sorted      | 5,333,728   | 1,353,587,755 | 4,529 |
| 12             | 12               | 3                |  valueEqualsValue7             | 5,459,288   | 1,378,107,143 | 3,515 |
| 12             | 12               | 3                |  valueEqualsValue3'Sorted      | 6,068,574   | 1,541,689,753 | 4,495 |
| 12             | 12               | 3                |  valueEqualsValue6             | 6,368,400   | 1,805,198,924 | 5,409 |
| 12             | 12               | 3                |  valueEqualsValue4'SmartSorted | 7,718,588   | 1,963,567,594 | 5,198 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue4             | 3,067,780   | 791,826,599   | 3,194 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue1             | 3,653,642   | 936,039,901   | 2,975 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue2             | 3,730,630   | 963,825,158   | 2,877 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue3             | 3,787,330   | 976,866,158   | 2,900 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue4'SmartSorted | 4,418,934   | 1,137,300,494 | 4,003 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue4'Sorted      | 4,451,186   | 1,139,542,302 | 3,793 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue5             | 4,549,876   | 1,456,314,175 | 3,305 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue7             | 4,549,876   | 1,456,314,175 | 3,305 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue1'Sorted      | 4,663,804   | 1,191,120,169 | 3,531 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue3'Sorted      | 5,199,850   | 1,333,498,167 | 3,460 |
| 12 (Plutonomy) | 12               | 3                |  valueEqualsValue6             | 5,451,476   | 1,585,921,338 | 4,248 |
| 13             | 13               | 3                |  valueEqualsValue4             | 3,660,332   | 939,456,907   | 4,143 |
| 13             | 13               | 3                |  valueEqualsValue1             | 4,408,300   | 1,121,874,291 | 3,901 |
| 13             | 13               | 3                |  valueEqualsValue2             | 4,562,596   | 1,168,418,491 | 3,824 |
| 13             | 13               | 3                |  valueEqualsValue3             | 4,597,196   | 1,176,376,491 | 3,849 |
| 13             | 13               | 3                |  valueEqualsValue4'Sorted      | 5,213,346   | 1,327,331,106 | 4,888 |
| 13             | 13               | 3                |  valueEqualsValue5             | 5,463,608   | 1,698,610,437 | 4,332 |
| 13             | 13               | 3                |  valueEqualsValue1'Sorted      | 5,488,866   | 1,393,673,736 | 4,529 |
| 13             | 13               | 3                |  valueEqualsValue7             | 5,660,326   | 1,428,804,624 | 3,515 |
| 13             | 13               | 3                |  valueEqualsValue3'Sorted      | 6,277,524   | 1,595,752,996 | 4,495 |
| 13             | 13               | 3                |  valueEqualsValue6             | 6,543,036   | 1,858,108,166 | 5,409 |
| 13             | 13               | 3                |  valueEqualsValue4'SmartSorted | 7,851,422   | 1,997,695,747 | 5,198 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue4             | 3,156,244   | 815,059,823   | 3,194 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue1             | 3,793,212   | 971,947,207   | 2,975 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue2             | 3,875,608   | 1,001,954,407 | 2,877 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue3             | 3,936,308   | 1,015,915,407 | 2,900 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue4'SmartSorted | 4,516,900   | 1,162,925,195 | 4,003 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue4'Sorted      | 4,572,958   | 1,171,585,022 | 3,793 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue5             | 4,681,020   | 1,510,158,353 | 3,305 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue7             | 4,681,020   | 1,510,158,353 | 3,305 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue1'Sorted      | 4,798,678   | 1,226,473,652 | 3,531 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue3'Sorted      | 5,380,036   | 1,380,873,912 | 3,460 |
| 13 (Plutonomy) | 13               | 3                |  valueEqualsValue6             | 5,601,448   | 1,633,086,082 | 4,248 |
| 14             | 14               | 3                |  valueEqualsValue4             | 3,763,760   | 966,203,629   | 4,143 |
| 14             | 14               | 3                |  valueEqualsValue1             | 4,571,034   | 1,163,181,095 | 3,901 |
| 14             | 14               | 3                |  valueEqualsValue2             | 4,738,840   | 1,214,107,629 | 3,824 |
| 14             | 14               | 3                |  valueEqualsValue3             | 4,775,940   | 1,222,640,629 | 3,849 |
| 14             | 14               | 3                |  valueEqualsValue4'Sorted      | 5,352,982   | 1,363,554,324 | 4,888 |
| 14             | 14               | 3                |  valueEqualsValue5             | 5,617,816   | 1,757,831,175 | 4,332 |
| 14             | 14               | 3                |  valueEqualsValue1'Sorted      | 5,644,004   | 1,433,759,717 | 4,529 |
| 14             | 14               | 3                |  valueEqualsValue7             | 5,891,170   | 1,487,248,278 | 3,515 |
| 14             | 14               | 3                |  valueEqualsValue3'Sorted      | 6,492,476   | 1,651,493,630 | 4,495 |
| 14             | 14               | 3                |  valueEqualsValue6             | 6,717,672   | 1,911,017,470 | 5,409 |
| 14             | 14               | 3                |  valueEqualsValue4'SmartSorted | 8,652,728   | 2,195,910,364 | 5,198 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue4             | 3,244,708   | 838,293,047   | 3,194 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue1             | 3,932,782   | 1,007,854,513 | 2,975 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue2             | 4,025,588   | 1,041,531,047 | 2,877 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue3             | 4,090,488   | 1,056,458,047 | 2,900 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue4'Sorted      | 4,694,730   | 1,203,627,742 | 3,793 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue5             | 4,812,164   | 1,564,002,593 | 3,305 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue7             | 4,812,164   | 1,564,002,593 | 3,305 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue4'SmartSorted | 4,816,290   | 1,238,323,384 | 4,003 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue1'Sorted      | 4,933,552   | 1,261,827,135 | 3,531 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue3'Sorted      | 5,565,424   | 1,429,743,048 | 3,460 |
| 14 (Plutonomy) | 14               | 3                |  valueEqualsValue6             | 5,751,420   | 1,680,250,888 | 4,248 |
| 15             | 15               | 3                |  valueEqualsValue4             | 3,867,188   | 992,950,351   | 4,143 |
| 15             | 15               | 3                |  valueEqualsValue1             | 4,733,768   | 1,204,487,899 | 3,901 |
| 15             | 15               | 3                |  valueEqualsValue2             | 4,920,886   | 1,261,428,158 | 3,824 |
| 15             | 15               | 3                |  valueEqualsValue3             | 4,960,686   | 1,270,582,158 | 3,849 |
| 15             | 15               | 3                |  valueEqualsValue4'Sorted      | 5,492,618   | 1,399,777,542 | 4,888 |
| 15             | 15               | 3                |  valueEqualsValue5             | 5,772,024   | 1,817,051,851 | 4,332 |
| 15             | 15               | 3                |  valueEqualsValue1'Sorted      | 5,799,142   | 1,473,845,698 | 4,529 |
| 15             | 15               | 3                |  valueEqualsValue7             | 6,151,820   | 1,553,438,105 | 3,515 |
| 15             | 15               | 3                |  valueEqualsValue3'Sorted      | 6,713,430   | 1,708,911,655 | 4,495 |
| 15             | 15               | 3                |  valueEqualsValue6             | 6,892,308   | 1,963,926,774 | 5,409 |
| 15             | 15               | 3                |  valueEqualsValue4'SmartSorted | 8,894,186   | 2,258,468,005 | 5,198 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue4             | 3,333,172   | 861,526,271   | 3,194 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue1             | 4,072,352   | 1,043,761,819 | 2,975 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue2             | 4,180,570   | 1,082,555,078 | 2,877 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue3             | 4,249,870   | 1,098,494,078 | 2,900 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue4'Sorted      | 4,816,502   | 1,235,670,462 | 3,793 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue5             | 4,943,308   | 1,617,846,771 | 3,305 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue7             | 4,943,308   | 1,617,846,771 | 3,305 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue4'SmartSorted | 4,946,864   | 1,272,596,581 | 4,003 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue1'Sorted      | 5,068,426   | 1,297,180,618 | 3,531 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue3'Sorted      | 5,756,014   | 1,480,105,575 | 3,460 |
| 15 (Plutonomy) | 15               | 3                |  valueEqualsValue6             | 5,901,392   | 1,727,415,694 | 4,248 |
| 16             | 16               | 4                |  valueEqualsValue4             | 4,229,724   | 1,082,903,602 | 4,143 |
| 16             | 16               | 4                |  valueEqualsValue1             | 5,135,806   | 1,303,860,352 | 3,901 |
| 16             | 16               | 4                |  valueEqualsValue2             | 5,306,930   | 1,357,803,163 | 3,824 |
| 16             | 16               | 4                |  valueEqualsValue3             | 5,349,030   | 1,367,486,163 | 3,849 |
| 16             | 16               | 4                |  valueEqualsValue4'Sorted      | 5,936,068   | 1,510,333,314 | 4,888 |
| 16             | 16               | 4                |  valueEqualsValue5             | 6,230,046   | 1,950,605,143 | 4,332 |
| 16             | 16               | 4                |  valueEqualsValue1'Sorted      | 6,258,094   | 1,588,264,233 | 4,529 |
| 16             | 16               | 4                |  valueEqualsValue7             | 6,534,654   | 1,647,786,299 | 3,515 |
| 16             | 16               | 4                |  valueEqualsValue3'Sorted      | 7,232,194   | 1,838,687,824 | 4,495 |
| 16             | 16               | 4                |  valueEqualsValue6             | 7,410,970   | 2,102,144,300 | 5,409 |
| 16             | 16               | 4                |  valueEqualsValue4'SmartSorted | 9,353,550   | 2,374,466,260 | 5,198 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue4             | 3,666,212   | 944,587,775   | 3,194 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue1             | 4,439,794   | 1,135,069,525 | 2,975 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue2             | 4,525,018   | 1,169,255,336 | 2,877 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue3             | 4,598,318   | 1,186,114,336 | 2,900 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue4'Sorted      | 5,227,756   | 1,338,713,487 | 3,793 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue4'SmartSorted | 5,304,316   | 1,362,908,467 | 4,003 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue5             | 5,362,534   | 1,742,369,316 | 3,305 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue7             | 5,362,534   | 1,742,369,316 | 3,305 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue1'Sorted      | 5,492,782   | 1,403,534,406 | 3,531 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue3'Sorted      | 6,221,882   | 1,597,607,997 | 3,460 |
| 16 (Plutonomy) | 16               | 4                |  valueEqualsValue6             | 6,373,058   | 1,854,716,473 | 4,248 |
| 17             | 17               | 4                |  valueEqualsValue4             | 4,413,964   | 1,130,002,866 | 4,143 |
| 17             | 17               | 4                |  valueEqualsValue1             | 5,332,746   | 1,353,925,329 | 3,901 |
| 17             | 17               | 4                |  valueEqualsValue2             | 5,516,778   | 1,411,815,455 | 3,824 |
| 17             | 17               | 4                |  valueEqualsValue3             | 5,561,378   | 1,422,073,455 | 3,849 |
| 17             | 17               | 4                |  valueEqualsValue4'Sorted      | 6,115,712   | 1,556,926,562 | 4,888 |
| 17             | 17               | 4                |  valueEqualsValue5             | 6,424,262   | 2,020,195,849 | 4,332 |
| 17             | 17               | 4                |  valueEqualsValue1'Sorted      | 6,453,240   | 1,638,720,244 | 4,529 |
| 17             | 17               | 4                |  valueEqualsValue7             | 6,768,296   | 1,706,576,562 | 3,515 |
| 17             | 17               | 4                |  valueEqualsValue3'Sorted      | 7,488,654   | 1,905,143,860 | 4,495 |
| 17             | 17               | 4                |  valueEqualsValue6             | 7,639,018   | 2,169,080,820 | 5,409 |
| 17             | 17               | 4                |  valueEqualsValue4'SmartSorted | 9,568,696   | 2,429,291,955 | 5,198 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue4             | 3,828,088   | 986,471,541   | 3,194 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue1             | 4,612,770   | 1,179,551,004 | 2,975 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue2             | 4,706,502   | 1,216,672,130 | 2,877 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue3             | 4,784,002   | 1,234,497,130 | 2,900 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue4'Sorted      | 5,388,336   | 1,380,850,237 | 3,793 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue4'SmartSorted | 5,476,494   | 1,407,367,710 | 4,003 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue5             | 5,533,186   | 1,806,468,524 | 3,305 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue7             | 5,533,186   | 1,806,468,524 | 3,305 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue1'Sorted      | 5,666,464   | 1,448,981,919 | 3,531 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue3'Sorted      | 6,446,178   | 1,656,594,535 | 3,460 |
| 17 (Plutonomy) | 17               | 4                |  valueEqualsValue6             | 6,573,742   | 1,915,287,495 | 4,248 |
| 18             | 18               | 4                |  valueEqualsValue4             | 4,594,304   | 1,176,205,130 | 4,143 |
| 18             | 18               | 4                |  valueEqualsValue1             | 5,529,686   | 1,403,990,306 | 3,901 |
| 18             | 18               | 4                |  valueEqualsValue2             | 5,732,428   | 1,467,459,138 | 3,824 |
| 18             | 18               | 4                |  valueEqualsValue3             | 5,779,728   | 1,478,338,138 | 3,849 |
| 18             | 18               | 4                |  valueEqualsValue4'Sorted      | 6,379,272   | 1,625,117,802 | 4,888 |
| 18             | 18               | 4                |  valueEqualsValue5             | 6,702,394   | 2,111,384,609 | 4,332 |
| 18             | 18               | 4                |  valueEqualsValue1'Sorted      | 6,732,302   | 1,710,774,247 | 4,529 |
| 18             | 18               | 4                |  valueEqualsValue7             | 7,031,744   | 1,773,112,998 | 3,515 |
| 18             | 18               | 4                |  valueEqualsValue3'Sorted      | 7,835,032   | 1,994,875,279 | 4,495 |
| 18             | 18               | 4                |  valueEqualsValue6             | 8,004,598   | 2,272,244,386 | 5,409 |
| 18             | 18               | 4                |  valueEqualsValue4'SmartSorted | 10,625,666  | 2,695,483,178 | 5,198 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue4             | 3,986,464   | 1,027,550,307 | 3,194 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue1             | 4,785,746   | 1,224,032,483 | 2,975 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue2             | 4,892,988   | 1,265,536,315 | 2,877 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue3             | 4,974,888   | 1,284,373,315 | 2,900 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue4'Sorted      | 5,624,932   | 1,442,767,979 | 3,793 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue5             | 5,779,154   | 1,890,187,786 | 3,305 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue7             | 5,779,154   | 1,890,187,786 | 3,305 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue4'SmartSorted | 5,830,100   | 1,497,517,529 | 4,003 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue1'Sorted      | 5,916,162   | 1,514,210,424 | 3,531 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue3'Sorted      | 6,750,992   | 1,736,694,456 | 3,460 |
| 18 (Plutonomy) | 18               | 4                |  valueEqualsValue6             | 6,894,558   | 2,008,083,563 | 4,248 |
| 19             | 19               | 4                |  valueEqualsValue4             | 4,774,644   | 1,222,407,394 | 4,143 |
| 19             | 19               | 4                |  valueEqualsValue1             | 5,726,626   | 1,454,055,283 | 3,901 |
| 19             | 19               | 4                |  valueEqualsValue2             | 5,953,880   | 1,524,734,212 | 3,824 |
| 19             | 19               | 4                |  valueEqualsValue3             | 6,004,080   | 1,536,280,212 | 3,849 |
| 19             | 19               | 4                |  valueEqualsValue4'Sorted      | 6,597,820   | 1,681,233,298 | 4,888 |
| 19             | 19               | 4                |  valueEqualsValue5             | 6,935,514   | 2,190,497,563 | 4,332 |
| 19             | 19               | 4                |  valueEqualsValue1'Sorted      | 6,966,352   | 1,770,752,506 | 4,529 |
| 19             | 19               | 4                |  valueEqualsValue7             | 7,324,998   | 1,847,395,607 | 3,515 |
| 19             | 19               | 4                |  valueEqualsValue3'Sorted      | 8,142,400   | 2,074,208,345 | 4,495 |
| 19             | 19               | 4                |  valueEqualsValue6             | 8,284,954   | 2,352,360,464 | 5,409 |
| 19             | 19               | 4                |  valueEqualsValue4'SmartSorted | 11,041,152  | 2,802,107,089 | 5,198 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue4             | 4,144,840   | 1,068,629,073 | 3,194 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue1             | 4,958,722   | 1,268,513,962 | 2,975 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue2             | 5,084,476   | 1,315,847,891 | 2,877 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue3             | 5,170,976   | 1,335,742,891 | 2,900 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue4'Sorted      | 5,822,316   | 1,493,943,977 | 3,793 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue5             | 5,985,910   | 1,963,165,242 | 3,305 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue7             | 5,985,910   | 1,963,165,242 | 3,305 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue4'SmartSorted | 6,035,686   | 1,550,786,004 | 4,003 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue1'Sorted      | 6,126,648   | 1,568,697,185 | 3,531 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue3'Sorted      | 7,021,796   | 1,807,546,024 | 3,460 |
| 19 (Plutonomy) | 19               | 4                |  valueEqualsValue6             | 7,142,550   | 2,080,684,143 | 4,248 |
| 20             | 20               | 4                |  valueEqualsValue4             | 4,954,984   | 1,268,609,658 | 4,143 |
| 20             | 20               | 4                |  valueEqualsValue1             | 5,923,566   | 1,504,120,260 | 3,901 |
| 20             | 20               | 4                |  valueEqualsValue2             | 6,181,134   | 1,583,640,677 | 3,824 |
| 20             | 20               | 4                |  valueEqualsValue3             | 6,234,434   | 1,595,899,677 | 3,849 |
| 20             | 20               | 4                |  valueEqualsValue4'Sorted      | 6,769,560   | 1,725,434,298 | 4,888 |
| 20             | 20               | 4                |  valueEqualsValue5             | 7,121,826   | 2,257,696,083 | 4,332 |
| 20             | 20               | 4                |  valueEqualsValue1'Sorted      | 7,153,594   | 1,818,816,269 | 4,529 |
| 20             | 20               | 4                |  valueEqualsValue7             | 7,648,058   | 1,929,424,389 | 3,515 |
| 20             | 20               | 4                |  valueEqualsValue3'Sorted      | 8,408,962   | 2,143,304,306 | 4,495 |
| 20             | 20               | 4                |  valueEqualsValue6             | 8,491,694   | 2,413,247,550 | 5,409 |
| 20             | 20               | 4                |  valueEqualsValue4'SmartSorted | 11,316,214  | 2,872,987,512 | 5,198 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue4             | 4,303,216   | 1,109,707,839 | 3,194 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue1             | 5,131,698   | 1,312,995,441 | 2,975 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue2             | 5,280,966   | 1,367,606,858 | 2,877 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue3             | 5,372,266   | 1,388,605,858 | 2,900 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue4'Sorted      | 5,976,192   | 1,533,964,479 | 3,793 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue5             | 6,149,158   | 2,024,987,264 | 3,305 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue7             | 6,149,158   | 2,024,987,264 | 3,305 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue4'SmartSorted | 6,197,764   | 1,592,898,983 | 4,003 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue1'Sorted      | 6,293,626   | 1,612,028,450 | 3,531 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue3'Sorted      | 7,254,294   | 1,868,735,487 | 3,460 |
| 20 (Plutonomy) | 20               | 4                |  valueEqualsValue6             | 7,324,626   | 2,135,826,731 | 4,248 |
| 21             | 21               | 5                |  valueEqualsValue4             | 5,417,610   | 1,382,621,730 | 4,143 |
| 21             | 21               | 5                |  valueEqualsValue1             | 6,412,892   | 1,624,273,332 | 3,901 |
| 21             | 21               | 5                |  valueEqualsValue2             | 6,651,968   | 1,700,547,229 | 3,824 |
| 21             | 21               | 5                |  valueEqualsValue3             | 6,707,968   | 1,713,427,229 | 3,849 |
| 21             | 21               | 5                |  valueEqualsValue4'Sorted      | 7,304,298   | 1,857,720,162 | 4,888 |
| 21             | 21               | 5                |  valueEqualsValue5             | 7,673,834   | 2,413,302,642 | 4,332 |
| 21             | 21               | 5                |  valueEqualsValue1'Sorted      | 7,707,532   | 1,955,518,133 | 4,529 |
| 21             | 21               | 5                |  valueEqualsValue7             | 8,312,612   | 2,094,113,905 | 3,515 |
| 21             | 21               | 5                |  valueEqualsValue3'Sorted      | 9,033,716   | 2,298,517,495 | 4,495 |
| 21             | 21               | 5                |  valueEqualsValue6             | 9,162,122   | 2,598,979,210 | 5,409 |
| 21             | 21               | 5                |  valueEqualsValue4'SmartSorted | 11,977,188  | 3,039,099,257 | 5,198 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue4             | 4,726,014   | 1,214,415,915 | 3,194 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue1             | 5,575,996   | 1,422,648,517 | 2,975 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue2             | 5,700,272   | 1,472,518,414 | 2,877 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue3             | 5,795,972   | 1,494,529,414 | 2,900 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue4'Sorted      | 6,464,502   | 1,655,428,347 | 3,793 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue5             | 6,641,738   | 2,166,781,827 | 3,305 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue7             | 6,641,738   | 2,166,781,827 | 3,305 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue4'SmartSorted | 6,660,880   | 1,709,265,758 | 4,003 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue1'Sorted      | 6,796,736   | 1,736,896,318 | 3,531 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue3'Sorted      | 7,803,920   | 2,006,525,680 | 3,460 |
| 21 (Plutonomy) | 21               | 5                |  valueEqualsValue6             | 7,918,026   | 2,303,698,395 | 4,248 |
| 22             | 22               | 5                |  valueEqualsValue4             | 5,521,038   | 1,409,368,452 | 4,143 |
| 22             | 22               | 5                |  valueEqualsValue1             | 6,619,430   | 1,676,249,662 | 3,901 |
| 22             | 22               | 5                |  valueEqualsValue2             | 6,828,212   | 1,746,237,111 | 3,824 |
| 22             | 22               | 5                |  valueEqualsValue3             | 6,886,712   | 1,759,692,111 | 3,849 |
| 22             | 22               | 5                |  valueEqualsValue4'Sorted      | 7,496,534   | 1,906,041,380 | 4,888 |
| 22             | 22               | 5                |  valueEqualsValue5             | 7,880,642   | 2,484,621,380 | 4,332 |
| 22             | 22               | 5                |  valueEqualsValue1'Sorted      | 7,915,270   | 2,007,702,114 | 4,529 |
| 22             | 22               | 5                |  valueEqualsValue7             | 8,483,844   | 2,137,065,213 | 3,515 |
| 22             | 22               | 5                |  valueEqualsValue3'Sorted      | 9,301,268   | 2,366,356,873 | 4,495 |
| 22             | 22               | 5                |  valueEqualsValue6             | 9,389,358   | 2,663,986,514 | 5,409 |
| 22             | 22               | 5                |  valueEqualsValue4'SmartSorted | 12,110,022  | 3,073,227,410 | 5,198 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue4             | 4,814,478   | 1,237,649,139 | 3,194 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue1             | 5,753,570   | 1,467,891,349 | 2,975 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue2             | 5,850,252   | 1,512,095,798 | 2,877 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue3             | 5,950,152   | 1,535,072,798 | 2,900 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue4'Sorted      | 6,637,874   | 1,699,339,067 | 3,793 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue4'SmartSorted | 6,758,846   | 1,734,890,459 | 4,003 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue5             | 6,823,082   | 2,232,172,067 | 3,305 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue7             | 6,823,082   | 2,232,172,067 | 3,305 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue1'Sorted      | 6,983,210   | 1,784,117,801 | 3,531 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue3'Sorted      | 8,039,508   | 2,066,941,560 | 3,460 |
| 22 (Plutonomy) | 22               | 5                |  valueEqualsValue6             | 8,118,198   | 2,362,409,201 | 4,248 |
| 23             | 23               | 5                |  valueEqualsValue4             | 5,624,466   | 1,436,115,174 | 4,143 |
| 23             | 23               | 5                |  valueEqualsValue1             | 6,825,968   | 1,728,225,992 | 3,901 |
| 23             | 23               | 5                |  valueEqualsValue2             | 7,010,258   | 1,793,558,384 | 3,824 |
| 23             | 23               | 5                |  valueEqualsValue3             | 7,071,458   | 1,807,634,384 | 3,849 |
| 23             | 23               | 5                |  valueEqualsValue4'Sorted      | 7,636,170   | 1,942,264,598 | 4,888 |
| 23             | 23               | 5                |  valueEqualsValue5             | 8,034,850   | 2,543,842,056 | 4,332 |
| 23             | 23               | 5                |  valueEqualsValue1'Sorted      | 8,070,408   | 2,047,788,095 | 4,529 |
| 23             | 23               | 5                |  valueEqualsValue7             | 8,684,882   | 2,187,762,694 | 3,515 |
| 23             | 23               | 5                |  valueEqualsValue3'Sorted      | 9,522,222   | 2,423,775,642 | 4,495 |
| 23             | 23               | 5                |  valueEqualsValue6             | 9,563,994   | 2,716,895,756 | 5,409 |
| 23             | 23               | 5                |  valueEqualsValue4'SmartSorted | 12,242,856  | 3,107,355,563 | 5,198 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue4             | 4,902,942   | 1,260,882,363 | 3,194 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue1             | 5,931,144   | 1,513,134,181 | 2,975 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue2             | 6,005,234   | 1,553,120,573 | 2,877 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue3             | 6,109,534   | 1,577,109,573 | 2,900 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue4'Sorted      | 6,759,646   | 1,731,381,787 | 3,793 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue4'SmartSorted | 6,856,812   | 1,760,515,160 | 4,003 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue5             | 6,954,226   | 2,286,016,245 | 3,305 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue7             | 6,954,226   | 2,286,016,245 | 3,305 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue1'Sorted      | 7,118,084   | 1,819,471,284 | 3,531 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue3'Sorted      | 8,230,098   | 2,117,304,831 | 3,460 |
| 23 (Plutonomy) | 23               | 5                |  valueEqualsValue6             | 8,268,170   | 2,409,573,945 | 4,248 |
| 24             | 24               | 5                |  valueEqualsValue4             | 5,727,894   | 1,462,861,896 | 4,143 |
| 24             | 24               | 5                |  valueEqualsValue1             | 7,032,506   | 1,780,202,322 | 3,901 |
| 24             | 24               | 5                |  valueEqualsValue2             | 7,198,106   | 1,842,511,048 | 3,824 |
| 24             | 24               | 5                |  valueEqualsValue3             | 7,262,206   | 1,857,254,048 | 3,849 |
| 24             | 24               | 5                |  valueEqualsValue4'Sorted      | 7,775,806   | 1,978,487,816 | 4,888 |
| 24             | 24               | 5                |  valueEqualsValue5             | 8,189,058   | 2,603,062,794 | 4,332 |
| 24             | 24               | 5                |  valueEqualsValue1'Sorted      | 8,225,546   | 2,087,874,076 | 4,529 |
| 24             | 24               | 5                |  valueEqualsValue7             | 8,915,726   | 2,246,206,348 | 3,515 |
| 24             | 24               | 5                |  valueEqualsValue6             | 9,738,630   | 2,769,805,060 | 5,409 |
| 24             | 24               | 5                |  valueEqualsValue3'Sorted      | 9,749,178   | 2,482,871,802 | 4,495 |
| 24             | 24               | 5                |  valueEqualsValue4'SmartSorted | 13,044,162  | 3,305,570,180 | 5,198 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue4             | 4,991,406   | 1,284,115,587 | 3,194 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue1             | 6,108,718   | 1,558,377,013 | 2,975 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue2             | 6,165,218   | 1,595,592,739 | 2,877 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue3             | 6,274,118   | 1,620,639,739 | 2,900 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue4'Sorted      | 6,881,418   | 1,763,424,507 | 3,793 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue5             | 7,085,370   | 2,339,860,485 | 3,305 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue7             | 7,085,370   | 2,339,860,485 | 3,305 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue4'SmartSorted | 7,156,202   | 1,835,913,349 | 4,003 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue1'Sorted      | 7,252,958   | 1,854,824,767 | 3,531 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue6             | 8,418,142   | 2,456,738,751 | 4,248 |
| 24 (Plutonomy) | 24               | 5                |  valueEqualsValue3'Sorted      | 8,425,890   | 2,169,161,493 | 3,460 |
| 25             | 25               | 5                |  valueEqualsValue4             | 5,831,322   | 1,489,608,618 | 4,143 |
| 25             | 25               | 5                |  valueEqualsValue1             | 7,239,044   | 1,832,178,652 | 3,901 |
| 25             | 25               | 5                |  valueEqualsValue2             | 7,391,756   | 1,893,095,103 | 3,824 |
| 25             | 25               | 5                |  valueEqualsValue3             | 7,458,956   | 1,908,551,103 | 3,849 |
| 25             | 25               | 5                |  valueEqualsValue4'Sorted      | 7,915,442   | 2,014,711,034 | 4,888 |
| 25             | 25               | 5                |  valueEqualsValue5             | 8,343,266   | 2,662,283,470 | 4,332 |
| 25             | 25               | 5                |  valueEqualsValue1'Sorted      | 8,380,684   | 2,127,960,057 | 4,529 |
| 25             | 25               | 5                |  valueEqualsValue7             | 9,176,376   | 2,312,396,175 | 3,515 |
| 25             | 25               | 5                |  valueEqualsValue6             | 9,913,266   | 2,822,714,364 | 5,409 |
| 25             | 25               | 5                |  valueEqualsValue3'Sorted      | 9,982,136   | 2,543,645,353 | 4,495 |
| 25             | 25               | 5                |  valueEqualsValue4'SmartSorted | 13,285,620  | 3,368,127,821 | 5,198 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue4             | 5,079,870   | 1,307,348,811 | 3,194 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue1             | 6,286,292   | 1,603,619,845 | 2,975 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue2             | 6,330,204   | 1,639,512,296 | 2,877 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue3             | 6,443,904   | 1,665,663,296 | 2,900 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue4'Sorted      | 7,003,190   | 1,795,467,227 | 3,793 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue5             | 7,216,514   | 2,393,704,663 | 3,305 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue7             | 7,216,514   | 2,393,704,663 | 3,305 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue4'SmartSorted | 7,286,776   | 1,870,186,546 | 4,003 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue1'Sorted      | 7,387,832   | 1,890,178,250 | 3,531 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue6             | 8,568,114   | 2,503,903,557 | 4,248 |
| 25 (Plutonomy) | 25               | 5                |  valueEqualsValue3'Sorted      | 8,626,884   | 2,222,511,546 | 3,460 |
| 26             | 26               | 6                |  valueEqualsValue4             | 6,290,048   | 1,602,723,690 | 4,143 |
| 26             | 26               | 6                |  valueEqualsValue1             | 7,728,370   | 1,952,331,724 | 3,901 |
| 26             | 26               | 6                |  valueEqualsValue2             | 7,868,392   | 2,011,633,418 | 3,824 |
| 26             | 26               | 6                |  valueEqualsValue3             | 7,938,492   | 2,027,756,418 | 3,849 |
| 26             | 26               | 6                |  valueEqualsValue4'Sorted      | 8,486,088   | 2,156,380,153 | 4,888 |
| 26             | 26               | 6                |  valueEqualsValue5             | 8,931,182   | 2,827,273,346 | 4,332 |
| 26             | 26               | 6                |  valueEqualsValue1'Sorted      | 8,970,530   | 2,274,045,176 | 4,529 |
| 26             | 26               | 6                |  valueEqualsValue7             | 9,870,736   | 2,484,832,980 | 3,515 |
| 26             | 26               | 6                |  valueEqualsValue6             | 10,646,410  | 3,025,119,534 | 5,409 |
| 26             | 26               | 6                |  valueEqualsValue3'Sorted      | 10,648,800  | 2,709,919,560 | 4,495 |
| 26             | 26               | 6                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue4             | 5,499,168   | 1,411,251,887 | 3,194 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue1             | 6,730,590   | 1,713,272,921 | 2,975 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue2             | 6,754,512   | 1,745,871,615 | 2,877 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue3             | 6,872,812   | 1,773,080,615 | 2,900 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue4'Sorted      | 7,523,808   | 1,925,486,350 | 3,793 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue5             | 7,741,402   | 2,544,054,543 | 3,305 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue7             | 7,741,402   | 2,544,054,543 | 3,305 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue4'SmartSorted | 7,782,200   | 1,995,108,576 | 4,003 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue1'Sorted      | 7,923,250   | 2,023,601,373 | 3,531 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue3'Sorted      | 9,214,020   | 2,370,350,757 | 3,460 |
| 26 (Plutonomy) | 26               | 6                |  valueEqualsValue6             | 9,216,230   | 2,686,608,731 | 4,248 |
| 27             | 27               | 6                |  valueEqualsValue4             | 6,393,476   | 1,629,470,412 | 4,143 |
| 27             | 27               | 6                |  valueEqualsValue1             | 7,934,908   | 2,004,308,054 | 3,901 |
| 27             | 27               | 6                |  valueEqualsValue2             | 8,050,438   | 2,058,955,063 | 3,824 |
| 27             | 27               | 6                |  valueEqualsValue3             | 8,123,238   | 2,075,699,063 | 3,849 |
| 27             | 27               | 6                |  valueEqualsValue4'Sorted      | 8,676,924   | 2,204,379,371 | 4,888 |
| 27             | 27               | 6                |  valueEqualsValue5             | 9,136,590   | 2,898,270,022 | 4,332 |
| 27             | 27               | 6                |  valueEqualsValue1'Sorted      | 9,176,868   | 2,325,907,157 | 4,529 |
| 27             | 27               | 6                |  valueEqualsValue7             | 10,041,968  | 2,527,784,288 | 3,515 |
| 27             | 27               | 6                |  valueEqualsValue6             | 10,872,246  | 3,089,804,838 | 5,409 |
| 27             | 27               | 6                |  valueEqualsValue3'Sorted      | 10,920,954  | 2,779,114,701 | 4,495 |
| 27             | 27               | 6                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue4             | 5,587,632   | 1,434,485,111 | 3,194 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue1             | 6,908,164   | 1,758,515,753 | 2,975 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue2             | 6,909,494   | 1,786,896,762 | 2,877 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue3             | 7,032,194   | 1,815,117,762 | 2,900 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue4'Sorted      | 7,695,380   | 1,968,983,070 | 3,793 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue4'SmartSorted | 7,880,166   | 2,020,733,277 | 4,003 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue5             | 7,920,946   | 2,609,030,721 | 3,305 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue7             | 7,920,946   | 2,609,030,721 | 3,305 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue1'Sorted      | 8,107,924   | 2,070,408,856 | 3,531 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue6             | 9,414,602   | 2,744,905,537 | 4,248 |
| 27 (Plutonomy) | 27               | 6                |  valueEqualsValue3'Sorted      | 9,453,010   | 2,431,846,400 | 3,460 |
| 28             | 28               | 6                |  valueEqualsValue4             | 6,496,904   | 1,656,217,134 | 4,143 |
| 28             | 28               | 6                |  valueEqualsValue1             | 8,141,446   | 2,056,284,384 | 3,901 |
| 28             | 28               | 6                |  valueEqualsValue2             | 8,238,286   | 2,107,908,099 | 3,824 |
| 28             | 28               | 6                |  valueEqualsValue3             | 8,313,986   | 2,125,319,099 | 3,849 |
| 28             | 28               | 6                |  valueEqualsValue4'Sorted      | 8,856,176   | 2,252,011,581 | 4,888 |
| 28             | 28               | 6                |  valueEqualsValue5             | 9,330,414   | 2,968,899,752 | 4,332 |
| 28             | 28               | 6                |  valueEqualsValue1'Sorted      | 9,371,622   | 2,377,402,130 | 4,529 |
| 28             | 28               | 6                |  valueEqualsValue7             | 10,243,006  | 2,578,481,769 | 3,515 |
| 28             | 28               | 6                |  valueEqualsValue6             | 11,140,114  | 3,168,752,064 | 5,409 |
| 28             | 28               | 6                |  valueEqualsValue3'Sorted      | 11,187,526  | 2,849,620,225 | 4,495 |
| 28             | 28               | 6                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue4             | 5,676,096   | 1,457,718,335 | 3,194 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue2             | 7,069,478   | 1,829,369,300 | 2,877 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue1             | 7,085,738   | 1,803,758,585 | 2,975 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue3             | 7,196,778   | 1,858,648,300 | 2,900 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue4'Sorted      | 7,850,768   | 2,011,054,782 | 3,793 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue4'SmartSorted | 7,978,132   | 2,046,357,978 | 4,003 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue5             | 8,087,106   | 2,673,225,953 | 3,305 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue7             | 8,087,106   | 2,673,225,953 | 3,305 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue1'Sorted      | 8,276,414   | 2,115,791,331 | 3,531 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue6             | 9,644,406   | 2,815,026,265 | 4,248 |
| 28 (Plutonomy) | 28               | 6                |  valueEqualsValue3'Sorted      | 9,683,818   | 2,494,054,426 | 3,460 |
| 29             | 29               | 6                |  valueEqualsValue4             | 6,600,332   | 1,682,963,856 | 4,143 |
| 29             | 29               | 6                |  valueEqualsValue1             | 8,347,984   | 2,108,260,714 | 3,901 |
| 29             | 29               | 6                |  valueEqualsValue2             | 8,431,936   | 2,158,492,526 | 3,824 |
| 29             | 29               | 6                |  valueEqualsValue3             | 8,510,736   | 2,176,616,526 | 3,849 |
| 29             | 29               | 6                |  valueEqualsValue4'Sorted      | 9,051,212   | 2,300,976,799 | 4,888 |
| 29             | 29               | 6                |  valueEqualsValue5             | 9,540,022   | 3,040,862,428 | 4,332 |
| 29             | 29               | 6                |  valueEqualsValue1'Sorted      | 9,582,160   | 2,430,230,111 | 4,529 |
| 29             | 29               | 6                |  valueEqualsValue7             | 10,473,850  | 2,636,925,423 | 3,515 |
| 29             | 29               | 6                |  valueEqualsValue6             | 11,370,150  | 3,234,403,368 | 5,409 |
| 29             | 29               | 6                |  valueEqualsValue3'Sorted      | 11,475,884  | 2,923,136,148 | 4,495 |
| 29             | 29               | 6                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue4             | 5,764,560   | 1,480,951,559 | 3,194 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue2             | 7,234,464   | 1,873,289,229 | 2,877 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue1             | 7,263,312   | 1,849,001,417 | 2,975 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue3             | 7,366,564   | 1,903,672,229 | 2,900 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue4'Sorted      | 8,027,340   | 2,055,701,502 | 3,793 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue5             | 8,271,650   | 2,739,352,131 | 3,305 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue7             | 8,271,650   | 2,739,352,131 | 3,305 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue4'SmartSorted | 8,364,138   | 2,143,975,159 | 4,003 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue1'Sorted      | 8,466,088   | 2,163,748,814 | 3,531 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue6             | 9,847,778   | 2,874,473,071 | 4,248 |
| 29 (Plutonomy) | 29               | 6                |  valueEqualsValue3'Sorted      | 9,938,212   | 2,559,686,851 | 3,460 |
| 30             | 30               | 6                |  valueEqualsValue4             | 6,703,760   | 1,709,710,578 | 4,143 |
| 30             | 30               | 6                |  valueEqualsValue1             | 8,554,522   | 2,160,237,044 | 3,901 |
| 30             | 30               | 6                |  valueEqualsValue2             | 8,631,388   | 2,210,708,344 | 3,824 |
| 30             | 30               | 6                |  valueEqualsValue3             | 8,713,488   | 2,229,591,344 | 3,849 |
| 30             | 30               | 6                |  valueEqualsValue4'Sorted      | 9,291,680   | 2,364,986,001 | 4,888 |
| 30             | 30               | 6                |  valueEqualsValue5             | 9,795,062   | 3,127,869,150 | 4,332 |
| 30             | 30               | 6                |  valueEqualsValue1'Sorted      | 9,838,130   | 2,498,102,076 | 4,529 |
| 30             | 30               | 6                |  valueEqualsValue7             | 10,734,500  | 2,703,115,250 | 3,515 |
| 30             | 30               | 6                |  valueEqualsValue6             | 11,752,850  | 3,344,356,640 | 5,409 |
| 30             | 30               | 6                |  valueEqualsValue3'Sorted      | 11,815,676  | 3,013,373,446 | 4,495 |
| 30             | 30               | 6                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue4             | 5,853,024   | 1,504,184,783 | 3,194 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue2             | 7,404,452   | 1,918,656,549 | 2,877 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue1             | 7,440,886   | 1,894,244,249 | 2,975 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue3             | 7,541,552   | 1,950,189,549 | 2,900 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue4'Sorted      | 8,236,344   | 2,112,402,206 | 3,793 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue5             | 8,491,426   | 2,818,176,355 | 3,305 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue7             | 8,491,426   | 2,818,176,355 | 3,305 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue4'SmartSorted | 8,581,944   | 2,202,906,340 | 4,003 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue1'Sorted      | 8,688,194   | 2,223,760,281 | 3,531 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue6             | 10,176,014  | 2,971,827,845 | 4,248 |
| 30 (Plutonomy) | 30               | 6                |  valueEqualsValue3'Sorted      | 10,233,040  | 2,639,510,651 | 3,460 |
| 31             | 31               | 7                |  valueEqualsValue4             | 7,162,486   | 1,822,825,650 | 4,143 |
| 31             | 31               | 7                |  valueEqualsValue1             | 9,043,848   | 2,280,390,116 | 3,901 |
| 31             | 31               | 7                |  valueEqualsValue2             | 9,113,826   | 2,330,878,422 | 3,824 |
| 31             | 31               | 7                |  valueEqualsValue3             | 9,199,026   | 2,350,474,422 | 3,849 |
| 31             | 31               | 7                |  valueEqualsValue4'Sorted      | 9,835,818   | 2,499,380,151 | 4,888 |
| 31             | 31               | 7                |  valueEqualsValue5             | 10,356,470  | 3,285,583,995 | 4,332 |
| 31             | 31               | 7                |  valueEqualsValue1'Sorted      | 10,401,468  | 2,636,912,226 | 4,529 |
| 31             | 31               | 7                |  valueEqualsValue7             | 11,458,666  | 2,883,299,344 | 3,515 |
| 31             | 31               | 7                |  valueEqualsValue6             | 12,432,678  | 3,532,142,872 | 5,409 |
| 31             | 31               | 7                |  valueEqualsValue3'Sorted      | 12,461,834  | 3,174,050,447 | 4,495 |
| 31             | 31               | 7                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue4             | 6,272,322   | 1,608,087,859 | 3,194 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue2             | 7,833,762   | 2,026,463,631 | 2,877 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue1             | 7,885,184   | 2,003,897,325 | 2,975 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue3             | 7,975,662   | 2,059,100,631 | 2,900 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue4'Sorted      | 8,734,454   | 2,236,066,360 | 3,793 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue5             | 8,993,806   | 2,962,171,204 | 3,305 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue7             | 8,993,806   | 2,962,171,204 | 3,305 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue4'SmartSorted | 9,054,860   | 2,321,473,401 | 4,003 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue1'Sorted      | 9,201,104   | 2,350,828,435 | 3,531 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue6             | 10,779,214  | 3,141,846,081 | 4,248 |
| 31 (Plutonomy) | 31               | 7                |  valueEqualsValue3'Sorted      | 10,802,870  | 2,782,488,656 | 3,460 |
| 32             | 32               | 7                |  valueEqualsValue4             | 7,265,914   | 1,849,572,372 | 4,143 |
| 32             | 32               | 7                |  valueEqualsValue1             | 9,250,386   | 2,332,366,446 | 3,901 |
| 32             | 32               | 7                |  valueEqualsValue2             | 9,301,674   | 2,379,831,830 | 3,824 |
| 32             | 32               | 7                |  valueEqualsValue3             | 9,389,774   | 2,400,094,830 | 3,849 |
| 32             | 32               | 7                |  valueEqualsValue4'Sorted      | 10,026,654  | 2,547,379,369 | 4,888 |
| 32             | 32               | 7                |  valueEqualsValue5             | 10,561,878  | 3,356,580,733 | 4,332 |
| 32             | 32               | 7                |  valueEqualsValue1'Sorted      | 10,607,806  | 2,688,774,207 | 4,529 |
| 32             | 32               | 7                |  valueEqualsValue7             | 11,629,898  | 2,926,250,652 | 3,515 |
| 32             | 32               | 7                |  valueEqualsValue6             | 12,658,514  | 3,596,828,176 | 5,409 |
| 32             | 32               | 7                |  valueEqualsValue3'Sorted      | 12,739,990  | 3,244,923,351 | 4,495 |
| 32             | 32               | 7                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue4             | 6,360,786   | 1,631,321,083 | 3,194 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue2             | 7,993,746   | 2,068,936,541 | 2,877 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue1             | 8,062,758   | 2,049,140,157 | 2,975 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue3             | 8,140,246   | 2,102,631,541 | 2,900 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue4'Sorted      | 8,906,026   | 2,279,563,080 | 3,793 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue4'SmartSorted | 9,152,826   | 2,347,098,102 | 4,003 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue5             | 9,173,350   | 3,027,147,444 | 3,305 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue7             | 9,173,350   | 3,027,147,444 | 3,305 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue1'Sorted      | 9,385,778   | 2,397,635,918 | 3,531 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue6             | 10,977,586  | 3,200,142,887 | 4,248 |
| 32 (Plutonomy) | 32               | 7                |  valueEqualsValue3'Sorted      | 11,047,062  | 2,845,478,062 | 3,460 |
| 33             | 33               | 7                |  valueEqualsValue4             | 7,369,342   | 1,876,319,094 | 4,143 |
| 33             | 33               | 7                |  valueEqualsValue1             | 9,456,924   | 2,384,342,776 | 3,901 |
| 33             | 33               | 7                |  valueEqualsValue2             | 9,495,324   | 2,430,416,629 | 3,824 |
| 33             | 33               | 7                |  valueEqualsValue3             | 9,586,524   | 2,451,392,629 | 3,849 |
| 33             | 33               | 7                |  valueEqualsValue4'Sorted      | 10,165,090  | 2,583,326,587 | 4,888 |
| 33             | 33               | 7                |  valueEqualsValue5             | 10,714,886  | 3,415,525,409 | 4,332 |
| 33             | 33               | 7                |  valueEqualsValue1'Sorted      | 10,761,744  | 2,728,584,188 | 4,529 |
| 33             | 33               | 7                |  valueEqualsValue7             | 11,830,936  | 2,976,948,133 | 3,515 |
| 33             | 33               | 7                |  valueEqualsValue6             | 12,831,950  | 3,649,461,480 | 5,409 |
| 33             | 33               | 7                |  valueEqualsValue3'Sorted      | 12,971,748  | 3,305,421,646 | 4,495 |
| 33             | 33               | 7                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue4             | 6,449,250   | 1,654,554,307 | 3,194 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue2             | 8,158,732   | 2,112,856,842 | 2,877 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue1             | 8,240,332   | 2,094,382,989 | 2,975 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue3             | 8,310,032   | 2,147,655,842 | 2,900 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue4'Sorted      | 9,026,598   | 2,311,329,800 | 3,793 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue4'SmartSorted | 9,250,792   | 2,372,722,803 | 4,003 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue5             | 9,303,294   | 3,080,715,622 | 3,305 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue7             | 9,303,294   | 3,080,715,622 | 3,305 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue1'Sorted      | 9,519,452   | 2,432,713,401 | 3,531 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue6             | 11,126,358  | 3,247,031,693 | 4,248 |
| 33 (Plutonomy) | 33               | 7                |  valueEqualsValue3'Sorted      | 11,246,856  | 2,898,552,859 | 3,460 |
| 34             | 34               | 7                |  valueEqualsValue4             | 7,472,770   | 1,903,065,816 | 4,143 |
| 34             | 34               | 7                |  valueEqualsValue1             | 9,663,462   | 2,436,319,106 | 3,901 |
| 34             | 34               | 7                |  valueEqualsValue2             | 9,694,776   | 2,482,632,819 | 3,824 |
| 34             | 34               | 7                |  valueEqualsValue3             | 9,789,276   | 2,504,367,819 | 3,849 |
| 34             | 34               | 7                |  valueEqualsValue4'Sorted      | 10,373,150  | 2,638,733,293 | 4,888 |
| 34             | 34               | 7                |  valueEqualsValue5             | 10,937,518  | 3,493,929,635 | 4,332 |
| 34             | 34               | 7                |  valueEqualsValue1'Sorted      | 10,985,306  | 2,787,853,657 | 4,529 |
| 34             | 34               | 7                |  valueEqualsValue7             | 12,061,780  | 3,035,391,787 | 3,515 |
| 34             | 34               | 7                |  valueEqualsValue6             | 13,155,434  | 3,743,497,698 | 5,409 |
| 34             | 34               | 7                |  valueEqualsValue3'Sorted      | 13,279,132  | 3,387,056,820 | 4,495 |
| 34             | 34               | 7                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue4             | 6,537,714   | 1,677,787,531 | 3,194 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue2             | 8,328,720   | 2,158,224,534 | 2,877 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue1             | 8,417,906   | 2,139,625,821 | 2,975 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue3             | 8,485,020   | 2,194,173,534 | 2,900 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue4'Sorted      | 9,207,194   | 2,360,348,008 | 3,793 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue5             | 9,494,662   | 3,151,857,350 | 3,305 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue7             | 9,494,662   | 3,151,857,350 | 3,305 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue4'SmartSorted | 9,606,006   | 2,464,406,480 | 4,003 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue1'Sorted      | 9,713,150   | 2,485,042,372 | 3,531 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue6             | 11,403,778  | 3,330,401,413 | 4,248 |
| 34 (Plutonomy) | 34               | 7                |  valueEqualsValue3'Sorted      | 11,513,276  | 2,970,694,535 | 3,460 |
| 35             | 35               | 7                |  valueEqualsValue4             | 7,576,198   | 1,929,812,538 | 4,143 |
| 35             | 35               | 7                |  valueEqualsValue1             | 9,870,000   | 2,488,295,436 | 3,901 |
| 35             | 35               | 7                |  valueEqualsValue2             | 9,900,030   | 2,536,480,400 | 3,824 |
| 35             | 35               | 7                |  valueEqualsValue3             | 9,998,030   | 2,559,020,400 | 3,849 |
| 35             | 35               | 7                |  valueEqualsValue4'Sorted      | 10,568,186  | 2,687,698,511 | 4,888 |
| 35             | 35               | 7                |  valueEqualsValue5             | 11,147,126  | 3,565,892,311 | 4,332 |
| 35             | 35               | 7                |  valueEqualsValue1'Sorted      | 11,195,844  | 2,840,681,638 | 4,529 |
| 35             | 35               | 7                |  valueEqualsValue7             | 12,322,430  | 3,101,581,614 | 3,515 |
| 35             | 35               | 7                |  valueEqualsValue6             | 13,385,470  | 3,809,149,002 | 5,409 |
| 35             | 35               | 7                |  valueEqualsValue3'Sorted      | 13,579,494  | 3,463,927,897 | 4,495 |
| 35             | 35               | 7                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue4             | 6,626,178   | 1,701,020,755 | 3,194 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue2             | 8,503,710   | 2,205,039,617 | 2,877 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue1             | 8,595,480   | 2,184,868,653 | 2,975 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue3             | 8,665,210   | 2,242,184,617 | 2,900 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue4'Sorted      | 9,383,766   | 2,404,994,728 | 3,793 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue5             | 9,679,206   | 3,217,983,528 | 3,305 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue7             | 9,679,206   | 3,217,983,528 | 3,305 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue4'SmartSorted | 9,791,380   | 2,511,283,677 | 4,003 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue1'Sorted      | 9,902,824   | 2,532,999,855 | 3,531 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue6             | 11,607,150  | 3,389,848,219 | 4,248 |
| 35 (Plutonomy) | 35               | 7                |  valueEqualsValue3'Sorted      | 11,778,074  | 3,039,314,114 | 3,460 |
| 36             | 36               | 8                |  valueEqualsValue4             | 8,034,924   | 2,042,927,610 | 4,143 |
| 36             | 36               | 8                |  valueEqualsValue1             | 10,359,326  | 2,608,448,508 | 3,901 |
| 36             | 36               | 8                |  valueEqualsValue2             | 10,388,270  | 2,658,282,241 | 3,824 |
| 36             | 36               | 8                |  valueEqualsValue3             | 10,489,570  | 2,681,581,241 | 3,849 |
| 36             | 36               | 8                |  valueEqualsValue4'Sorted      | 11,076,920  | 2,813,400,962 | 4,888 |
| 36             | 36               | 8                |  valueEqualsValue5             | 11,673,130  | 3,714,915,519 | 4,332 |
| 36             | 36               | 8                |  valueEqualsValue1'Sorted      | 11,723,778  | 2,970,800,089 | 4,529 |
| 36             | 36               | 8                |  valueEqualsValue7             | 13,076,402  | 3,289,512,997 | 3,515 |
| 36             | 36               | 8                |  valueEqualsValue3'Sorted      |  Eval Error |               | 4,495 |
| 36             | 36               | 8                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 36             | 36               | 8                |  valueEqualsValue6             |  Eval Error |               | 5,409 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue4             | 7,045,476   | 1,804,923,831 | 3,194 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue2             | 8,938,022   | 2,314,294,462 | 2,877 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue1             | 9,039,778   | 2,294,521,729 | 2,975 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue3             | 9,104,522   | 2,352,589,462 | 2,900 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue4'Sorted      | 9,847,772   | 2,520,266,183 | 3,793 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue5             | 10,147,482  | 3,353,585,740 | 3,305 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue7             | 10,147,482  | 3,353,585,740 | 3,305 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue4'SmartSorted | 10,230,192  | 2,621,458,039 | 4,003 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue1'Sorted      | 10,381,630  | 2,651,675,310 | 3,531 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue6             | 12,165,042  | 3,548,348,057 | 4,248 |
| 36 (Plutonomy) | 36               | 8                |  valueEqualsValue3'Sorted      | 12,319,002  | 3,175,393,183 | 3,460 |
| 37             | 37               | 8                |  valueEqualsValue4             | 8,138,352   | 2,069,674,332 | 4,143 |
| 37             | 37               | 8                |  valueEqualsValue1             | 10,565,864  | 2,660,424,838 | 3,901 |
| 37             | 37               | 8                |  valueEqualsValue2             | 10,581,920  | 2,708,867,412 | 3,824 |
| 37             | 37               | 8                |  valueEqualsValue3             | 10,686,320  | 2,732,879,412 | 3,849 |
| 37             | 37               | 8                |  valueEqualsValue4'Sorted      | 11,269,156  | 2,861,722,180 | 4,888 |
| 37             | 37               | 8                |  valueEqualsValue5             | 11,879,938  | 3,786,234,195 | 4,332 |
| 37             | 37               | 8                |  valueEqualsValue1'Sorted      | 11,931,516  | 3,022,984,070 | 4,529 |
| 37             | 37               | 8                |  valueEqualsValue7             | 13,247,634  | 3,332,464,305 | 3,515 |
| 37             | 37               | 8                |  valueEqualsValue3'Sorted      |  Eval Error |               | 4,495 |
| 37             | 37               | 8                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 37             | 37               | 8                |  valueEqualsValue6             |  Eval Error |               | 5,409 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue4             | 7,133,940   | 1,828,157,055 | 3,194 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue2             | 9,103,008   | 2,358,215,135 | 2,877 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue1             | 9,217,352   | 2,339,764,561 | 2,975 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue3             | 9,274,308   | 2,397,614,135 | 2,900 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue4'Sorted      | 10,021,144  | 2,564,176,903 | 3,793 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue4'SmartSorted | 10,328,158  | 2,647,082,740 | 4,003 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue5             | 10,328,826  | 3,418,975,918 | 3,305 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue7             | 10,328,826  | 3,418,975,918 | 3,305 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue1'Sorted      | 10,568,104  | 2,698,896,793 | 3,531 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue6             | 12,365,214  | 3,607,058,863 | 4,248 |
| 37 (Plutonomy) | 37               | 8                |  valueEqualsValue3'Sorted      | 12,570,196  | 3,240,290,352 | 3,460 |
| 38             | 38               | 8                |  valueEqualsValue4             | 8,241,780   | 2,096,421,054 | 4,143 |
| 38             | 38               | 8                |  valueEqualsValue1             | 10,772,402  | 2,712,401,168 | 3,901 |
| 38             | 38               | 8                |  valueEqualsValue2             | 10,781,372  | 2,761,083,974 | 3,824 |
| 38             | 38               | 8                |  valueEqualsValue3             | 10,889,072  | 2,785,854,974 | 3,849 |
| 38             | 38               | 8                |  valueEqualsValue4'Sorted      | 11,452,008  | 2,910,182,390 | 4,888 |
| 38             | 38               | 8                |  valueEqualsValue5             | 12,077,362  | 3,857,691,925 | 4,332 |
| 38             | 38               | 8                |  valueEqualsValue1'Sorted      | 12,129,870  | 3,075,307,043 | 4,529 |
| 38             | 38               | 8                |  valueEqualsValue7             | 13,448,672  | 3,383,161,786 | 3,515 |
| 38             | 38               | 8                |  valueEqualsValue3'Sorted      |  Eval Error |               | 4,495 |
| 38             | 38               | 8                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 38             | 38               | 8                |  valueEqualsValue6             |  Eval Error |               | 5,409 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue4             | 7,222,404   | 1,851,390,279 | 3,194 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue2             | 9,272,996   | 2,403,583,199 | 2,877 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue1             | 9,394,926   | 2,385,007,393 | 2,975 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue3             | 9,449,296   | 2,444,132,199 | 2,900 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue4'Sorted      | 10,179,732  | 2,606,984,615 | 3,793 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue4'SmartSorted | 10,426,124  | 2,672,707,441 | 4,003 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue5             | 10,498,186  | 3,483,907,150 | 3,305 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue7             | 10,498,186  | 3,483,907,150 | 3,305 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue1'Sorted      | 10,739,794  | 2,745,015,268 | 3,531 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue6             | 12,598,218  | 3,677,915,653 | 4,248 |
| 38 (Plutonomy) | 38               | 8                |  valueEqualsValue3'Sorted      | 12,814,608  | 3,306,221,904 | 3,460 |
| 39             | 39               | 8                |  valueEqualsValue4             | 8,345,208   | 2,123,167,776 | 4,143 |
| 39             | 39               | 8                |  valueEqualsValue1             | 10,978,940  | 2,764,377,498 | 3,901 |
| 39             | 39               | 8                |  valueEqualsValue2             | 10,986,626  | 2,814,931,927 | 3,824 |
| 39             | 39               | 8                |  valueEqualsValue3             | 11,097,826  | 2,840,507,927 | 3,849 |
| 39             | 39               | 8                |  valueEqualsValue4'Sorted      | 11,675,852  | 2,966,922,104 | 4,888 |
| 39             | 39               | 8                |  valueEqualsValue5             | 12,315,778  | 3,937,429,097 | 4,332 |
| 39             | 39               | 8                |  valueEqualsValue1'Sorted      | 12,369,216  | 3,135,909,520 | 4,529 |
| 39             | 39               | 8                |  valueEqualsValue7             | 13,679,516  | 3,441,605,440 | 3,515 |
| 39             | 39               | 8                |  valueEqualsValue3'Sorted      |  Eval Error |               | 4,495 |
| 39             | 39               | 8                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 39             | 39               | 8                |  valueEqualsValue6             |  Eval Error |               | 5,409 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue4             | 7,310,868   | 1,874,623,503 | 3,194 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue2             | 9,447,986   | 2,450,398,654 | 2,877 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue1             | 9,572,500   | 2,430,250,225 | 2,975 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue3             | 9,629,486   | 2,492,143,654 | 2,900 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue4'Sorted      | 10,381,512  | 2,658,577,831 | 3,793 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue5             | 10,707,938  | 3,556,979,824 | 3,305 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue7             | 10,707,938  | 3,556,979,824 | 3,305 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue4'SmartSorted | 10,842,338  | 2,778,421,118 | 4,003 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue1'Sorted      | 10,954,676  | 2,799,919,247 | 3,531 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue6             | 12,849,206  | 3,750,611,389 | 4,248 |
| 39 (Plutonomy) | 39               | 8                |  valueEqualsValue3'Sorted      | 13,104,614  | 3,381,788,351 | 3,460 |
| 40             | 40               | 8                |  valueEqualsValue4             | 8,448,636   | 2,149,914,498 | 4,143 |
| 40             | 40               | 8                |  valueEqualsValue1             | 11,185,478  | 2,816,353,828 | 3,901 |
| 40             | 40               | 8                |  valueEqualsValue2             | 11,197,682  | 2,870,411,271 | 3,824 |
| 40             | 40               | 8                |  valueEqualsValue3             | 11,312,582  | 2,896,838,271 | 3,849 |
| 40             | 40               | 8                |  valueEqualsValue4'Sorted      | 11,858,704  | 3,015,382,314 | 4,888 |
| 40             | 40               | 8                |  valueEqualsValue5             | 12,513,202  | 4,008,886,827 | 4,332 |
| 40             | 40               | 8                |  valueEqualsValue1'Sorted      | 12,567,570  | 3,188,232,493 | 4,529 |
| 40             | 40               | 8                |  valueEqualsValue7             | 13,940,166  | 3,507,795,267 | 3,515 |
| 40             | 40               | 8                |  valueEqualsValue3'Sorted      |  Eval Error |               | 4,495 |
| 40             | 40               | 8                |  valueEqualsValue4'SmartSorted |  Eval Error |               | 5,198 |
| 40             | 40               | 8                |  valueEqualsValue6             |  Eval Error |               | 5,409 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue4             | 7,399,332   | 1,897,856,727 | 3,194 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue2             | 9,627,978   | 2,498,661,500 | 2,877 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue1             | 9,750,074   | 2,475,493,057 | 2,975 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue3             | 9,814,878   | 2,541,648,500 | 2,900 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue4'Sorted      | 10,540,100  | 2,701,385,543 | 3,793 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue5             | 10,877,298  | 3,621,911,056 | 3,305 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue7             | 10,877,298  | 3,621,911,056 | 3,305 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue4'SmartSorted | 11,009,728  | 2,823,459,307 | 4,003 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue1'Sorted      | 11,126,366  | 2,846,037,722 | 3,531 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue6             | 13,082,210  | 3,821,468,179 | 4,248 |
| 40 (Plutonomy) | 40               | 8                |  valueEqualsValue3'Sorted      | 13,359,430  | 3,450,706,685 | 3,460 |

From the results above you can see that the method valueEqualsValue4 is the best in terms of efficiency. 

It uses maps and converts them to partial lists only when necessary, without flattening all the values.

First, it converts the values to lists:
```
listCS1 = TxAssocMap.toList mp1
listCS2 = TxAssocMap.toList mp2
```

The result is a list of this type: [(CurrencySymbol, Map TokenName Integer)].

It then searches for each currency symbol in the first list in the other list. When a currency symbol is found in both lists, it compares the maps inside each one, converting them to lists, and if they are equal, it deletes the currency symbol from the lists and continues with the next one.

This is an improvement over the more general approach of flattening values in larger lists of this type: [CurrencySymbol, TokenName, Integer].

It makes sense that converting the maps to lists only when needed can help reduce unnecessary computation. Also, searching for currency symbols on smaller lists before searching for token names on smaller sublists is a good optimization to reduce the search space.

After valueEqualsValue4, valueEqualsValue1 and valueEqualsValue2 are among the best ones. 

valueEqualsValue1 is flattening the values to larger lists of this type: [CurrencySymbol, TokenName, Integer]. It also removes elements from the lists as well.
