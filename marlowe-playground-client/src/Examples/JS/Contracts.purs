module Examples.JS.Contracts where

example :: String
example =
  """
    return Close;

"""

escrow :: String
escrow =
  """
    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const buyer: Party = Role("Buyer");
    const seller: Party = Role("Seller");
    const arbiter: Party = Role("Arbiter");

    const price: Value = ConstantParam("Price");

    const depositTimeout: Timeout = SlotParam("Buyer's deposit timeout");
    const disputeTimeout: Timeout = SlotParam("Buyer's dispute timeout");
    const answerTimeout: Timeout = SlotParam("Seller's response timeout");
    const arbitrageTimeout: Timeout = SlotParam("Timeout for arbitrage");

    function choice(choiceName: string, chooser: Party, choiceValue: SomeNumber, continuation: Contract): Case {
        return Case(Choice(ChoiceId(choiceName, chooser),
            [Bound(choiceValue, choiceValue)]),
            continuation);
    }


    function deposit(timeout: Timeout, timeoutContinuation: Contract, continuation: Contract): Contract {
        return When([Case(Deposit(seller, buyer, ada, price), continuation)],
            timeout,
            timeoutContinuation);
    }

    function choices(timeout: Timeout, chooser: Party, timeoutContinuation: Contract, list: { value: SomeNumber, name: string, continuation: Contract }[]): Contract {
        var caseList: Case[] = new Array(list.length);
        list.forEach((element, index) =>
            caseList[index] = choice(element.name, chooser, element.value, element.continuation)
        );
        return When(caseList, timeout, timeoutContinuation);
    }

    function sellerToBuyer(continuation: Contract): Contract {
        return Pay(seller, Account(buyer), ada, price, continuation);
    }

    function paySeller(continuation: Contract): Contract {
        return Pay(buyer, Party(seller), ada, price, continuation);
    }

    const refundBuyer: Contract = explicitRefunds ? Pay(buyer, Party(buyer), ada, price, Close) : Close;

    const refundSeller: Contract = explicitRefunds ? Pay(seller, Party(seller), ada, price, Close) : Close;

    const contract: Contract =
        deposit(depositTimeout, Close,
            choices(disputeTimeout, buyer, refundSeller,
                [{ value: 0n, name: "Everything is alright", continuation: refundSeller },
                {
                    value: 1n, name: "Report problem",
                    continuation:
                        sellerToBuyer(
                            choices(answerTimeout, seller, refundBuyer,
                                [{ value: 1n, name: "Confirm problem", continuation: refundBuyer },
                                {
                                    value: 0n, name: "Dispute problem", continuation:
                                        choices(arbitrageTimeout, arbiter, refundBuyer,
                                            [{ value: 0n, name: "Dismiss claim", continuation: paySeller(Close) },
                                            { value: 1n, name: "Confirm problem", continuation: refundBuyer }
                                            ])
                                }]))
                }]));

    return contract;

"""

escrowWithCollateral :: String
escrowWithCollateral =
  """
    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const buyer: Party = Role("Buyer");
    const seller: Party = Role("Seller");
    const burnAddress: Party = PK("0000000000000000000000000000000000000000000000000000000000000000");

    const price: Value = ConstantParam("Price");
    const collateral: Value = ConstantParam("Collateral amount");

    const sellerCollateralTimeout: Timeout = SlotParam("Collateral deposit by seller timeout");
    const buyerCollateralTimeout: Timeout = SlotParam("Deposit of collateral by buyer timeout");
    const depositTimeout: Timeout = SlotParam("Deposit of price by buyer timeout");
    const disputeTimeout: Timeout = SlotParam("Dispute by buyer timeout");
    const answerTimeout: Timeout = SlotParam("Seller's response timeout");

    function depositCollateral(party: Party, timeout: Timeout, timeoutContinuation: Contract, continuation: Contract): Contract {
        return When([Case(Deposit(party, party, ada, collateral), continuation)],
            timeout,
            timeoutContinuation);
    }

    function burnCollaterals(continuation: Contract): Contract {
        return Pay(seller, Party(burnAddress), ada, collateral,
            Pay(buyer, Party(burnAddress), ada, collateral,
                continuation));
    }

    function deposit(timeout: Timeout, timeoutContinuation: Contract, continuation: Contract): Contract {
        return When([Case(Deposit(seller, buyer, ada, price), continuation)],
            timeout,
            timeoutContinuation);
    }

    function choice(choiceName: string, chooser: Party, choiceValue: SomeNumber, continuation: Contract): Case {
        return Case(Choice(ChoiceId(choiceName, chooser),
            [Bound(choiceValue, choiceValue)]),
            continuation);
    }

    function choices(timeout: Timeout, chooser: Party, timeoutContinuation: Contract, list: { value: SomeNumber, name: string, continuation: Contract }[]): Contract {
        var caseList: Case[] = new Array(list.length);
        list.forEach((element, index) =>
            caseList[index] = choice(element.name, chooser, element.value, element.continuation)
        );
        return When(caseList, timeout, timeoutContinuation);
    }

    function sellerToBuyer(continuation: Contract): Contract {
        return Pay(seller, Account(buyer), ada, price, continuation);
    }

    function refundSellerCollateral(continuation: Contract): Contract {
        if (explicitRefunds) {
            return Pay(seller, Party(seller), ada, collateral, continuation);
        } else {
            return continuation;
        }
    }

    function refundBuyerCollateral(continuation: Contract): Contract {
        if (explicitRefunds) {
            return Pay(buyer, Party(buyer), ada, collateral, continuation);
        } else {
            return continuation;
        }
    }

    function refundCollaterals(continuation: Contract): Contract {
        return refundSellerCollateral(refundBuyerCollateral(continuation));
    }

    const refundBuyer: Contract = explicitRefunds ? Pay(buyer, Party(buyer), ada, price, Close) : Close;

    const refundSeller: Contract = explicitRefunds ? Pay(seller, Party(seller), ada, price, Close) : Close;

    const contract: Contract =
        depositCollateral(seller, sellerCollateralTimeout, Close,
            depositCollateral(buyer, buyerCollateralTimeout, refundSellerCollateral(Close),
                deposit(depositTimeout, refundCollaterals(Close),
                    choices(disputeTimeout, buyer, refundCollaterals(refundSeller),
                        [{ value: 0n, name: "Everything is alright", continuation: refundCollaterals(refundSeller) },
                        {
                            value: 1n, name: "Report problem",
                            continuation:
                                sellerToBuyer(
                                    choices(answerTimeout, seller, refundCollaterals(refundBuyer),
                                        [{ value: 1n, name: "Confirm problem", continuation: refundCollaterals(refundBuyer) },
                                        { value: 0n, name: "Dispute problem", continuation: burnCollaterals(refundBuyer) }]))
                        }]))));

    return contract;

"""

zeroCouponBond :: String
zeroCouponBond =
  """
    const discountedPrice: Value = ConstantParam("Discounted price");
    const notionalPrice: Value = ConstantParam("Notional price");

    const investor: Party = Role("Investor");
    const issuer: Party = Role("Issuer");

    const initialExchange: Timeout = SlotParam("Initial exchange deadline");
    const maturityExchangeTimeout: Timeout = SlotParam("Maturity exchange deadline");

    function transfer(timeout: Timeout, from: Party, to: Party, amount: Value, continuation: Contract): Contract {
        return When([Case(Deposit(from, from, ada, amount),
            Pay(from, Party(to), ada, amount, continuation))],
            timeout,
            Close);
    }

    const contract: Contract =
        transfer(initialExchange, investor, issuer, discountedPrice,
            transfer(maturityExchangeTimeout, issuer, investor, notionalPrice,
                Close))

    return contract;

"""

couponBondGuaranteed :: String
couponBondGuaranteed =
  """
    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const guarantor: Party = Role("Guarantor");
    const investor: Party = Role("Investor");
    const issuer: Party = Role("Issuer");

    const principal: Value = ConstantParam("Principal");
    const instalment: Value = ConstantParam("Interest instalment");

    function guaranteedAmount(instalments: SomeNumber): Value {
        return AddValue(MulValue(Constant(instalments), instalment), principal);
    }

    const lastInstalment: Value = AddValue(instalment, principal);

    function deposit(amount: Value, by: Party, toAccount: Party,
        timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return When([Case(Deposit(toAccount, by, ada, amount), continuation)],
            timeout,
            timeoutContinuation);
    }

    function refundGuarantor(amount: Value, continuation: Contract): Contract {
        return Pay(investor, Party(guarantor), ada, amount, continuation)
    }

    function transfer(amount: Value, from: Party, to: Party,
        timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return deposit(amount, from, to, timeout, timeoutContinuation,
            Pay(to, Party(to), ada, amount,
                continuation))
    }

    function giveCollateralToInvestor(amount: Value): Contract {
        if (explicitRefunds) {
            return Pay(investor, Party(investor), ada, amount,
                Close);
        } else {
            return Close;
        }
    }

    const contract: Contract =
        deposit(guaranteedAmount(3n), guarantor, investor,
            300n, Close,
            transfer(principal, investor, issuer,
                600n, refundGuarantor(guaranteedAmount(3n), Close),
                transfer(instalment, issuer, investor,
                    900n, giveCollateralToInvestor(guaranteedAmount(3n)),
                    refundGuarantor(instalment,
                        transfer(instalment, issuer, investor,
                            1200n, giveCollateralToInvestor(guaranteedAmount(2n)),
                            refundGuarantor(instalment,
                                transfer(lastInstalment, issuer, investor,
                                    1500n, giveCollateralToInvestor(guaranteedAmount(1n)),
                                    refundGuarantor(lastInstalment,
                                        Close))))))));

    return contract;

"""

swap :: String
swap =
  """
    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const lovelacePerAda: Value = Constant(1000000n);
    const amountOfAda: Value = ConstantParam("Amount of Ada");
    const amountOfLovelace: Value = MulValue(lovelacePerAda, amountOfAda);
    const amountOfDollars: Value = ConstantParam("Amount of dollars");

    const adaDepositTimeout: Timeout = SlotParam("Timeout for Ada deposit");
    const dollarDepositTimeout: Timeout = SlotParam("Timeout for dollar deposit");

    const dollars: Token = Token("85bb65", "dollar")

    type SwapParty = {
        party: Party;
        currency: Token;
        amount: Value;
    };

    const adaProvider: SwapParty = {
        party: Role("Ada provider"),
        currency: ada,
        amount: amountOfLovelace
    }

    const dollarProvider: SwapParty = {
        party: Role("Dollar provider"),
        currency: dollars,
        amount: amountOfDollars
    }

    function makeDeposit(src: SwapParty, timeout: Timeout,
        timeoutContinuation: Contract, continuation: Contract): Contract {
        return When([Case(Deposit(src.party, src.party, src.currency, src.amount),
            continuation)],
            timeout,
            timeoutContinuation);
    }

    function refundSwapParty(party: SwapParty): Contract {
        if (explicitRefunds) {
            return Pay(party.party, Party(party.party), party.currency, party.amount, Close);
        } else {
            return Close;
        }
    }

    const makePayment = function (src: SwapParty, dest: SwapParty,
        continuation: Contract): Contract {
        return Pay(src.party, Party(dest.party), src.currency, src.amount,
            continuation);
    }

    const contract: Contract = makeDeposit(adaProvider, adaDepositTimeout, Close,
        makeDeposit(dollarProvider, dollarDepositTimeout, refundSwapParty(adaProvider),
            makePayment(adaProvider, dollarProvider,
                makePayment(dollarProvider, adaProvider,
                    Close))))

    return contract;

"""

contractForDifferences :: String
contractForDifferences =
  """
    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const party: Party = Role("Party");
    const counterparty: Party = Role("Counterparty");
    const oracle: Party = Role("Oracle");

    const depositAmount: bigint = 100_000_000n;
    const deposit: Value = Constant(depositAmount);
    const doubleDeposit: Value = Constant(depositAmount * 2n);

    const priceBeginning: ChoiceId = ChoiceId("Price at beginning", oracle);
    const priceEnd: ChoiceId = ChoiceId("Price at end", oracle);

    const decreaseInPrice: ValueId = "Decrease in price";
    const increaseInPrice: ValueId = "Increase in price";

    function initialDeposit(by: Party, timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return When([Case(Deposit(by, by, ada, deposit), continuation)],
            timeout,
            timeoutContinuation);
    }

    function oracleInput(choiceId: ChoiceId, timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return When([Case(Choice(choiceId, [Bound(0n, 1_000_000_000n)]), continuation)],
            timeout, timeoutContinuation);
    }

    function wait(timeout: ETimeout, continuation: Contract): Contract {
        return When([], timeout, continuation);
    }

    function gtLtEq(value1: Value, value2: Value, gtContinuation: Contract,
        ltContinuation: Contract, eqContinuation: Contract): Contract {
        return If(ValueGT(value1, value2), gtContinuation
            , If(ValueLT(value1, value2), ltContinuation,
                eqContinuation))
    }

    function recordDifference(name: ValueId, choiceId1: ChoiceId, choiceId2: ChoiceId,
        continuation: Contract): Contract {
        return Let(name, SubValue(ChoiceValue(choiceId1), ChoiceValue(choiceId2)),
            continuation);
    }

    function transferUpToDeposit(from: Party, to: Party, amount: Value, continuation: Contract): Contract {
        return Pay(from, Account(to), ada, Cond(ValueLT(amount, deposit), amount, deposit), continuation);
    }

    function refund(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return Pay(who, Party(who), ada, amount,
                continuation);
        } else {
            return continuation;
        }
    }

    const refundBoth: Contract = refund(party, deposit, refund(counterparty, deposit, Close));

    function refundIfGtZero(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return If(ValueGT(amount, Constant(0n)), refund(who, amount, continuation), continuation);
        } else {
            return continuation;
        }
    }

    function refundUpToDoubleOfDeposit(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return refund(who, Cond(ValueGT(amount, doubleDeposit), doubleDeposit, amount),
                continuation);
        } else {
            return continuation;
        }
    }

    function refundAfterDifference(payer: Party, payee: Party, difference: Value): Contract {
        return refundIfGtZero(payer, SubValue(deposit, difference),
            refundUpToDoubleOfDeposit(payee, AddValue(deposit, difference),
                Close));
    }

    const contract: Contract =
        initialDeposit(party, 300n, Close,
            initialDeposit(counterparty, 600n, refund(party, deposit, Close),
                oracleInput(priceBeginning, 900n, refundBoth,
                    wait(1500n,
                        oracleInput(priceEnd, 1800n, refundBoth,
                            gtLtEq(ChoiceValue(priceBeginning), ChoiceValue(priceEnd),
                                recordDifference(decreaseInPrice, priceBeginning, priceEnd,
                                    transferUpToDeposit(counterparty, party, UseValue(decreaseInPrice),
                                        refundAfterDifference(counterparty, party, UseValue(decreaseInPrice)))),
                                recordDifference(increaseInPrice, priceEnd, priceBeginning,
                                    transferUpToDeposit(party, counterparty, UseValue(increaseInPrice),
                                        refundAfterDifference(party, counterparty, UseValue(increaseInPrice)))),
                                refundBoth
                            ))))));

    return contract;

"""

contractForDifferencesWithOracle :: String
contractForDifferencesWithOracle =
  """

    /* We can set explicitRefunds true to run Close refund analysis
       but we get a shorter contract if we set it to false */
    const explicitRefunds: Boolean = false;

    const party: Party = Role("Party");
    const counterparty: Party = Role("Counterparty");
    const oracle: Party = Role("kraken");

    const partyDepositAmount: bigint = 100_000_000n;
    const counterpartyDepositAmount: bigint = 100_000_000n;
    const partyDeposit: Value = Constant(partyDepositAmount);
    const counterpartyDeposit: Value = Constant(counterpartyDepositAmount);
    const bothDeposits: Value = Constant(partyDepositAmount + counterpartyDepositAmount);

    const priceBeginning: Value = Constant(100_000_000n);
    const priceEnd: ValueId = ValueId("Price at end");

    const exchangeBeginning: ChoiceId = ChoiceId("dir-adausd", oracle);
    const exchangeEnd: ChoiceId = ChoiceId("inv-adausd", oracle);

    const decreaseInPrice: ValueId = "Decrease in price";
    const increaseInPrice: ValueId = "Increase in price";

    function initialDeposit(by: Party, deposit: Value, timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return When([Case(Deposit(by, by, ada, deposit), continuation)],
            timeout,
            timeoutContinuation);
    }

    function oracleInput(choiceId: ChoiceId, timeout: ETimeout, timeoutContinuation: Contract,
        continuation: Contract): Contract {
        return When([Case(Choice(choiceId, [Bound(0n, 100_000_000_000n)]), continuation)],
            timeout, timeoutContinuation);
    }

    function wait(timeout: ETimeout, continuation: Contract): Contract {
        return When([], timeout, continuation);
    }

    function gtLtEq(value1: Value, value2: Value, gtContinuation: Contract,
        ltContinuation: Contract, eqContinuation: Contract): Contract {
        return If(ValueGT(value1, value2), gtContinuation
            , If(ValueLT(value1, value2), ltContinuation,
                eqContinuation))
    }

    function recordEndPrice(name: ValueId, choiceId1: ChoiceId, choiceId2: ChoiceId,
        continuation: Contract): Contract {
        return Let(name, Scale(1n, 10_000_000_000_000_000n, MulValue(priceBeginning, MulValue(ChoiceValue(choiceId1), ChoiceValue(choiceId2)))),
            continuation);
    }

    function recordDifference(name: ValueId, val1: Value, val2: Value,
        continuation: Contract): Contract {
        return Let(name, SubValue(val1, val2), continuation);
    }

    function transferUpToDeposit(from: Party, payerDeposit: Value, to: Party, amount: Value, continuation: Contract): Contract {
        return Pay(from, Account(to), ada, Cond(ValueLT(amount, payerDeposit), amount, payerDeposit), continuation);
    }

    function refund(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return Pay(who, Party(who), ada, amount,
                continuation);
        } else {
            return continuation;
        }
    }

    const refundBoth: Contract = refund(party, partyDeposit, refund(counterparty, counterpartyDeposit, Close));

    function refundIfGtZero(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return If(ValueGT(amount, Constant(0n)), refund(who, amount, continuation), continuation);
        } else {
            return continuation;
        }
    }

    function refundUpToBothDeposits(who: Party, amount: Value, continuation: Contract): Contract {
        if (explicitRefunds) {
            return refund(who, Cond(ValueGT(amount, bothDeposits), bothDeposits, amount),
                continuation);
        } else {
            return continuation;
        }
    }

    function refundAfterDifference(payer: Party, payerDeposit: Value, payee: Party, payeeDeposit: Value, difference: Value): Contract {
        return refundIfGtZero(payer, SubValue(payerDeposit, difference),
            refundUpToBothDeposits(payee, AddValue(payeeDeposit, difference),
                Close));
    }

    const contract: Contract =
        initialDeposit(party, partyDeposit, 300n, Close,
            initialDeposit(counterparty, counterpartyDeposit, 600n, refund(party, partyDeposit, Close),
                oracleInput(exchangeBeginning, 900n, refundBoth,
                    wait(1500n,
                        oracleInput(exchangeEnd, 1800n, refundBoth,
                            recordEndPrice(priceEnd, exchangeBeginning, exchangeEnd,
                                gtLtEq(priceBeginning, UseValue(priceEnd),
                                    recordDifference(decreaseInPrice, priceBeginning, UseValue(priceEnd),
                                        transferUpToDeposit(counterparty, counterpartyDeposit, party, UseValue(decreaseInPrice),
                                            refundAfterDifference(counterparty, counterpartyDeposit, party, partyDeposit, UseValue(decreaseInPrice)))),
                                    recordDifference(increaseInPrice, UseValue(priceEnd), priceBeginning,
                                        transferUpToDeposit(party, partyDeposit, counterparty, UseValue(increaseInPrice),
                                            refundAfterDifference(party, partyDeposit, counterparty, counterpartyDeposit, UseValue(increaseInPrice)))),
                                    refundBoth
                                )))))));

    return contract;
"""
