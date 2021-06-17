(program
  (let
    (nonrec)
    (datatypebind
      (datatype
        (tyvardecl CampaignAction (type))

        CampaignAction_match
        (vardecl Collect CampaignAction) (vardecl Refund CampaignAction)
      )
    )
    (datatypebind
      (datatype
        (tyvardecl StakingCredential (type))

        StakingCredential_match
        (vardecl StakingHash (fun (con bytestring) StakingCredential))
        (vardecl
          StakingPtr
          (fun (con integer) (fun (con integer) (fun (con integer) StakingCredential)))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl DCert (type))

        DCert_match
        (vardecl DCertDelegDeRegKey (fun StakingCredential DCert))
        (vardecl
          DCertDelegDelegate
          (fun StakingCredential (fun (con bytestring) DCert))
        )
        (vardecl DCertDelegRegKey (fun StakingCredential DCert))
        (vardecl DCertGenesis DCert)
        (vardecl DCertMir DCert)
        (vardecl
          DCertPoolRegister (fun (con bytestring) (fun (con bytestring) DCert))
        )
        (vardecl
          DCertPoolRetire (fun (con bytestring) (fun (con integer) DCert))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef (type))

        TxOutRef_match
        (vardecl TxOutRef (fun (con bytestring) (fun (con integer) TxOutRef)))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl ScriptPurpose (type))

        ScriptPurpose_match
        (vardecl Certifying (fun DCert ScriptPurpose))
        (vardecl Minting (fun (con bytestring) ScriptPurpose))
        (vardecl Rewarding (fun StakingCredential ScriptPurpose))
        (vardecl Spending (fun TxOutRef ScriptPurpose))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
        (tyvardecl a (type)) (tyvardecl b (type))
        Tuple2_match
        (vardecl Tuple2 (fun a (fun b [[Tuple2 a] b])))
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List (fun (type) (type)))
          (tyvardecl a (type))
          Nil_match
          (vardecl Nil [List a]) (vardecl Cons (fun a (fun [List a] [List a])))
        )
      )
      (let
        (rec)
        (datatypebind
          (datatype
            (tyvardecl Data (type))

            Data_match
            (vardecl B (fun (con bytestring) Data))
            (vardecl Constr (fun (con integer) (fun [List Data] Data)))
            (vardecl I (fun (con integer) Data))
            (vardecl List (fun [List Data] Data))
            (vardecl Map (fun [List [[Tuple2 Data] Data]] Data))
          )
        )
        (let
          (nonrec)
          (datatypebind
            (datatype
              (tyvardecl Extended (fun (type) (type)))
              (tyvardecl a (type))
              Extended_match
              (vardecl Finite (fun a [Extended a]))
              (vardecl NegInf [Extended a])
              (vardecl PosInf [Extended a])
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Bool (type))

              Bool_match
              (vardecl True Bool) (vardecl False Bool)
            )
          )
          (datatypebind
            (datatype
              (tyvardecl LowerBound (fun (type) (type)))
              (tyvardecl a (type))
              LowerBound_match
              (vardecl LowerBound (fun [Extended a] (fun Bool [LowerBound a])))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl UpperBound (fun (type) (type)))
              (tyvardecl a (type))
              UpperBound_match
              (vardecl UpperBound (fun [Extended a] (fun Bool [UpperBound a])))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Interval (fun (type) (type)))
              (tyvardecl a (type))
              Interval_match
              (vardecl
                Interval (fun [LowerBound a] (fun [UpperBound a] [Interval a]))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Credential (type))

              Credential_match
              (vardecl PubKeyCredential (fun (con bytestring) Credential))
              (vardecl ScriptCredential (fun (con bytestring) Credential))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Maybe (fun (type) (type)))
              (tyvardecl a (type))
              Maybe_match
              (vardecl Just (fun a [Maybe a])) (vardecl Nothing [Maybe a])
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Address (type))

              Address_match
              (vardecl
                Address (fun Credential (fun [Maybe StakingCredential] Address))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxOut (type))

              TxOut_match
              (vardecl
                TxOut
                (fun Address (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [Maybe (con bytestring)] TxOut)))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxInInfo (type))

              TxInInfo_match
              (vardecl TxInInfo (fun TxOutRef (fun TxOut TxInInfo)))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxInfo (type))

              TxInfo_match
              (vardecl
                TxInfo
                (fun [List TxInInfo] (fun [List TxOut] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [List DCert] (fun [List [[Tuple2 StakingCredential] (con integer)]] (fun [Interval (con integer)] (fun [List (con bytestring)] (fun [List [[Tuple2 (con bytestring)] Data]] (fun (con bytestring) TxInfo))))))))))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl ScriptContext (type))

              ScriptContext_match
              (vardecl
                ScriptContext (fun TxInfo (fun ScriptPurpose ScriptContext))
              )
            )
          )
          (termbind
            (strict)
            (vardecl equalsInteger (fun (con integer) (fun (con integer) Bool)))
            (lam
              arg
              (con integer)
              (lam
                arg
                (con integer)
                (let
                  (nonrec)
                  (termbind
                    (strict)
                    (vardecl b (con bool))
                    [ [ (builtin equalsInteger) arg ] arg ]
                  )
                  [ [ [ { (builtin ifThenElse) Bool } b ] True ] False ]
                )
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              lessThanEqInteger (fun (con integer) (fun (con integer) Bool))
            )
            (lam
              arg
              (con integer)
              (lam
                arg
                (con integer)
                (let
                  (nonrec)
                  (termbind
                    (strict)
                    (vardecl b (con bool))
                    [ [ (builtin lessThanEqualsInteger) arg ] arg ]
                  )
                  [ [ [ { (builtin ifThenElse) Bool } b ] True ] False ]
                )
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Ordering (type))

              Ordering_match
              (vardecl EQ Ordering) (vardecl GT Ordering) (vardecl LT Ordering)
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Ord (fun (type) (type)))
              (tyvardecl a (type))
              Ord_match
              (vardecl
                CConsOrd
                (fun [(lam a (type) (fun a (fun a Bool))) a] (fun (fun a (fun a Ordering)) (fun (fun a (fun a Bool)) (fun (fun a (fun a Bool)) (fun (fun a (fun a Bool)) (fun (fun a (fun a Bool)) (fun (fun a (fun a a)) (fun (fun a (fun a a)) [Ord a]))))))))
              )
            )
          )
          (datatypebind
            (datatype (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit))
          )
          (termbind
            (nonstrict)
            (vardecl fOrdPOSIXTime [Ord (con integer)])
            [
              [
                [
                  [
                    [
                      [
                        [
                          [ { CConsOrd (con integer) } equalsInteger ]
                          (lam
                            x
                            (con integer)
                            (lam
                              y
                              (con integer)
                              [
                                [
                                  [
                                    {
                                      [ Bool_match [ [ equalsInteger x ] y ] ]
                                      (fun Unit Ordering)
                                    }
                                    (lam thunk Unit EQ)
                                  ]
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      [
                                        [
                                          {
                                            [
                                              Bool_match
                                              [ [ lessThanEqInteger x ] y ]
                                            ]
                                            (fun Unit Ordering)
                                          }
                                          (lam thunk Unit LT)
                                        ]
                                        (lam thunk Unit GT)
                                      ]
                                      Unit
                                    ]
                                  )
                                ]
                                Unit
                              ]
                            )
                          )
                        ]
                        (lam
                          arg
                          (con integer)
                          (lam
                            arg
                            (con integer)
                            (let
                              (nonrec)
                              (termbind
                                (strict)
                                (vardecl b (con bool))
                                [ [ (builtin lessThanInteger) arg ] arg ]
                              )
                              [
                                [ [ { (builtin ifThenElse) Bool } b ] True ]
                                False
                              ]
                            )
                          )
                        )
                      ]
                      lessThanEqInteger
                    ]
                    (lam
                      arg
                      (con integer)
                      (lam
                        arg
                        (con integer)
                        (let
                          (nonrec)
                          (termbind
                            (strict)
                            (vardecl b (con bool))
                            [ [ (builtin greaterThanInteger) arg ] arg ]
                          )
                          [ [ [ { (builtin ifThenElse) Bool } b ] True ] False ]
                        )
                      )
                    )
                  ]
                  (lam
                    arg
                    (con integer)
                    (lam
                      arg
                      (con integer)
                      (let
                        (nonrec)
                        (termbind
                          (strict)
                          (vardecl b (con bool))
                          [ [ (builtin greaterThanEqualsInteger) arg ] arg ]
                        )
                        [ [ [ { (builtin ifThenElse) Bool } b ] True ] False ]
                      )
                    )
                  )
                ]
                (lam
                  x
                  (con integer)
                  (lam
                    y
                    (con integer)
                    [
                      [
                        [
                          {
                            [ Bool_match [ [ lessThanEqInteger x ] y ] ]
                            (fun Unit (con integer))
                          }
                          (lam thunk Unit y)
                        ]
                        (lam thunk Unit x)
                      ]
                      Unit
                    ]
                  )
                )
              ]
              (lam
                x
                (con integer)
                (lam
                  y
                  (con integer)
                  [
                    [
                      [
                        {
                          [ Bool_match [ [ lessThanEqInteger x ] y ] ]
                          (fun Unit (con integer))
                        }
                        (lam thunk Unit x)
                      ]
                      (lam thunk Unit y)
                    ]
                    Unit
                  ]
                )
              )
            ]
          )
          (datatypebind
            (datatype
              (tyvardecl Campaign (type))

              Campaign_match
              (vardecl
                Campaign
                (fun (con integer) (fun (con integer) (fun (con bytestring) Campaign)))
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              compare (all a (type) (fun [Ord a] (fun a (fun a Ordering))))
            )
            (abs
              a
              (type)
              (lam
                v
                [Ord a]
                [
                  { [ { Ord_match a } v ] (fun a (fun a Ordering)) }
                  (lam
                    v
                    [(lam a (type) (fun a (fun a Bool))) a]
                    (lam
                      v
                      (fun a (fun a Ordering))
                      (lam
                        v
                        (fun a (fun a Bool))
                        (lam
                          v
                          (fun a (fun a Bool))
                          (lam
                            v
                            (fun a (fun a Bool))
                            (lam
                              v
                              (fun a (fun a Bool))
                              (lam
                                v (fun a (fun a a)) (lam v (fun a (fun a a)) v)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ]
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              hull_ccompare
              (all a (type) (fun [Ord a] (fun [Extended a] (fun [Extended a] Ordering))))
            )
            (abs
              a
              (type)
              (lam
                dOrd
                [Ord a]
                (lam
                  ds
                  [Extended a]
                  (lam
                    ds
                    [Extended a]
                    (let
                      (nonrec)
                      (termbind
                        (strict)
                        (vardecl fail (fun (all a (type) a) Ordering))
                        (lam ds (all a (type) a) (error Ordering))
                      )
                      [
                        [
                          [
                            [
                              {
                                [ { Extended_match a } ds ] (fun Unit Ordering)
                              }
                              (lam
                                default_arg0
                                a
                                (lam
                                  thunk
                                  Unit
                                  [
                                    [
                                      [
                                        [
                                          {
                                            [ { Extended_match a } ds ]
                                            (fun Unit Ordering)
                                          }
                                          (lam
                                            default_arg0
                                            a
                                            (lam
                                              thunk
                                              Unit
                                              (let
                                                (nonrec)
                                                (termbind
                                                  (strict)
                                                  (vardecl
                                                    fail
                                                    (fun (all a (type) a) Ordering)
                                                  )
                                                  (lam
                                                    ds
                                                    (all a (type) a)
                                                    [
                                                      [
                                                        [
                                                          [
                                                            {
                                                              [
                                                                {
                                                                  Extended_match
                                                                  a
                                                                }
                                                                ds
                                                              ]
                                                              (fun Unit Ordering)
                                                            }
                                                            (lam
                                                              default_arg0
                                                              a
                                                              (lam
                                                                thunk
                                                                Unit
                                                                [
                                                                  [
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            {
                                                                              Extended_match
                                                                              a
                                                                            }
                                                                            ds
                                                                          ]
                                                                          (fun Unit Ordering)
                                                                        }
                                                                        (lam
                                                                          l
                                                                          a
                                                                          (lam
                                                                            thunk
                                                                            Unit
                                                                            [
                                                                              [
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      [
                                                                                        {
                                                                                          Extended_match
                                                                                          a
                                                                                        }
                                                                                        ds
                                                                                      ]
                                                                                      (fun Unit Ordering)
                                                                                    }
                                                                                    (lam
                                                                                      r
                                                                                      a
                                                                                      (lam
                                                                                        thunk
                                                                                        Unit
                                                                                        [
                                                                                          [
                                                                                            [
                                                                                              {
                                                                                                compare
                                                                                                a
                                                                                              }
                                                                                              dOrd
                                                                                            ]
                                                                                            l
                                                                                          ]
                                                                                          r
                                                                                        ]
                                                                                      )
                                                                                    )
                                                                                  ]
                                                                                  (lam
                                                                                    thunk
                                                                                    Unit
                                                                                    [
                                                                                      fail
                                                                                      (abs
                                                                                        e
                                                                                        (type)
                                                                                        (error
                                                                                          e
                                                                                        )
                                                                                      )
                                                                                    ]
                                                                                  )
                                                                                ]
                                                                                (lam
                                                                                  thunk
                                                                                  Unit
                                                                                  [
                                                                                    fail
                                                                                    (abs
                                                                                      e
                                                                                      (type)
                                                                                      (error
                                                                                        e
                                                                                      )
                                                                                    )
                                                                                  ]
                                                                                )
                                                                              ]
                                                                              Unit
                                                                            ]
                                                                          )
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          fail
                                                                          (abs
                                                                            e
                                                                            (type)
                                                                            (error
                                                                              e
                                                                            )
                                                                          )
                                                                        ]
                                                                      )
                                                                    ]
                                                                    (lam
                                                                      thunk
                                                                      Unit
                                                                      GT
                                                                    )
                                                                  ]
                                                                  Unit
                                                                ]
                                                              )
                                                            )
                                                          ]
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              [
                                                                [
                                                                  [
                                                                    {
                                                                      [
                                                                        {
                                                                          Extended_match
                                                                          a
                                                                        }
                                                                        ds
                                                                      ]
                                                                      (fun Unit Ordering)
                                                                    }
                                                                    (lam
                                                                      l
                                                                      a
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          [
                                                                            [
                                                                              [
                                                                                {
                                                                                  [
                                                                                    {
                                                                                      Extended_match
                                                                                      a
                                                                                    }
                                                                                    ds
                                                                                  ]
                                                                                  (fun Unit Ordering)
                                                                                }
                                                                                (lam
                                                                                  r
                                                                                  a
                                                                                  (lam
                                                                                    thunk
                                                                                    Unit
                                                                                    [
                                                                                      [
                                                                                        [
                                                                                          {
                                                                                            compare
                                                                                            a
                                                                                          }
                                                                                          dOrd
                                                                                        ]
                                                                                        l
                                                                                      ]
                                                                                      r
                                                                                    ]
                                                                                  )
                                                                                )
                                                                              ]
                                                                              (lam
                                                                                thunk
                                                                                Unit
                                                                                [
                                                                                  fail
                                                                                  (abs
                                                                                    e
                                                                                    (type)
                                                                                    (error
                                                                                      e
                                                                                    )
                                                                                  )
                                                                                ]
                                                                              )
                                                                            ]
                                                                            (lam
                                                                              thunk
                                                                              Unit
                                                                              [
                                                                                fail
                                                                                (abs
                                                                                  e
                                                                                  (type)
                                                                                  (error
                                                                                    e
                                                                                  )
                                                                                )
                                                                              ]
                                                                            )
                                                                          ]
                                                                          Unit
                                                                        ]
                                                                      )
                                                                    )
                                                                  ]
                                                                  (lam
                                                                    thunk
                                                                    Unit
                                                                    [
                                                                      fail
                                                                      (abs
                                                                        e
                                                                        (type)
                                                                        (error e
                                                                        )
                                                                      )
                                                                    ]
                                                                  )
                                                                ]
                                                                (lam
                                                                  thunk Unit GT
                                                                )
                                                              ]
                                                              Unit
                                                            ]
                                                          )
                                                        ]
                                                        (lam thunk Unit LT)
                                                      ]
                                                      Unit
                                                    ]
                                                  )
                                                )
                                                [
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (fun Unit Ordering)
                                                        }
                                                        (lam
                                                          default_arg0
                                                          a
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              fail
                                                              (abs
                                                                e
                                                                (type)
                                                                (error e)
                                                              )
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (lam
                                                        thunk
                                                        Unit
                                                        [
                                                          fail
                                                          (abs
                                                            e (type) (error e)
                                                          )
                                                        ]
                                                      )
                                                    ]
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        [
                                                          [
                                                            [
                                                              {
                                                                [
                                                                  {
                                                                    Extended_match
                                                                    a
                                                                  }
                                                                  ds
                                                                ]
                                                                (fun Unit Ordering)
                                                              }
                                                              (lam
                                                                default_arg0
                                                                a
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    fail
                                                                    (abs
                                                                      e
                                                                      (type)
                                                                      (error e)
                                                                    )
                                                                  ]
                                                                )
                                                              )
                                                            ]
                                                            (lam
                                                              thunk
                                                              Unit
                                                              [
                                                                fail
                                                                (abs
                                                                  e
                                                                  (type)
                                                                  (error e)
                                                                )
                                                              ]
                                                            )
                                                          ]
                                                          (lam thunk Unit EQ)
                                                        ]
                                                        Unit
                                                      ]
                                                    )
                                                  ]
                                                  Unit
                                                ]
                                              )
                                            )
                                          )
                                        ]
                                        (lam thunk Unit GT)
                                      ]
                                      (lam
                                        thunk
                                        Unit
                                        (let
                                          (nonrec)
                                          (termbind
                                            (strict)
                                            (vardecl
                                              fail
                                              (fun (all a (type) a) Ordering)
                                            )
                                            (lam
                                              ds
                                              (all a (type) a)
                                              [
                                                [
                                                  [
                                                    [
                                                      {
                                                        [
                                                          { Extended_match a }
                                                          ds
                                                        ]
                                                        (fun Unit Ordering)
                                                      }
                                                      (lam
                                                        default_arg0
                                                        a
                                                        (lam
                                                          thunk
                                                          Unit
                                                          [
                                                            [
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      {
                                                                        Extended_match
                                                                        a
                                                                      }
                                                                      ds
                                                                    ]
                                                                    (fun Unit Ordering)
                                                                  }
                                                                  (lam
                                                                    l
                                                                    a
                                                                    (lam
                                                                      thunk
                                                                      Unit
                                                                      [
                                                                        [
                                                                          [
                                                                            [
                                                                              {
                                                                                [
                                                                                  {
                                                                                    Extended_match
                                                                                    a
                                                                                  }
                                                                                  ds
                                                                                ]
                                                                                (fun Unit Ordering)
                                                                              }
                                                                              (lam
                                                                                r
                                                                                a
                                                                                (lam
                                                                                  thunk
                                                                                  Unit
                                                                                  [
                                                                                    [
                                                                                      [
                                                                                        {
                                                                                          compare
                                                                                          a
                                                                                        }
                                                                                        dOrd
                                                                                      ]
                                                                                      l
                                                                                    ]
                                                                                    r
                                                                                  ]
                                                                                )
                                                                              )
                                                                            ]
                                                                            (lam
                                                                              thunk
                                                                              Unit
                                                                              [
                                                                                fail
                                                                                (abs
                                                                                  e
                                                                                  (type)
                                                                                  (error
                                                                                    e
                                                                                  )
                                                                                )
                                                                              ]
                                                                            )
                                                                          ]
                                                                          (lam
                                                                            thunk
                                                                            Unit
                                                                            [
                                                                              fail
                                                                              (abs
                                                                                e
                                                                                (type)
                                                                                (error
                                                                                  e
                                                                                )
                                                                              )
                                                                            ]
                                                                          )
                                                                        ]
                                                                        Unit
                                                                      ]
                                                                    )
                                                                  )
                                                                ]
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    fail
                                                                    (abs
                                                                      e
                                                                      (type)
                                                                      (error e)
                                                                    )
                                                                  ]
                                                                )
                                                              ]
                                                              (lam thunk Unit GT
                                                              )
                                                            ]
                                                            Unit
                                                          ]
                                                        )
                                                      )
                                                    ]
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        [
                                                          [
                                                            [
                                                              {
                                                                [
                                                                  {
                                                                    Extended_match
                                                                    a
                                                                  }
                                                                  ds
                                                                ]
                                                                (fun Unit Ordering)
                                                              }
                                                              (lam
                                                                l
                                                                a
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    [
                                                                      [
                                                                        [
                                                                          {
                                                                            [
                                                                              {
                                                                                Extended_match
                                                                                a
                                                                              }
                                                                              ds
                                                                            ]
                                                                            (fun Unit Ordering)
                                                                          }
                                                                          (lam
                                                                            r
                                                                            a
                                                                            (lam
                                                                              thunk
                                                                              Unit
                                                                              [
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      compare
                                                                                      a
                                                                                    }
                                                                                    dOrd
                                                                                  ]
                                                                                  l
                                                                                ]
                                                                                r
                                                                              ]
                                                                            )
                                                                          )
                                                                        ]
                                                                        (lam
                                                                          thunk
                                                                          Unit
                                                                          [
                                                                            fail
                                                                            (abs
                                                                              e
                                                                              (type)
                                                                              (error
                                                                                e
                                                                              )
                                                                            )
                                                                          ]
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          fail
                                                                          (abs
                                                                            e
                                                                            (type)
                                                                            (error
                                                                              e
                                                                            )
                                                                          )
                                                                        ]
                                                                      )
                                                                    ]
                                                                    Unit
                                                                  ]
                                                                )
                                                              )
                                                            ]
                                                            (lam
                                                              thunk
                                                              Unit
                                                              [
                                                                fail
                                                                (abs
                                                                  e
                                                                  (type)
                                                                  (error e)
                                                                )
                                                              ]
                                                            )
                                                          ]
                                                          (lam thunk Unit GT)
                                                        ]
                                                        Unit
                                                      ]
                                                    )
                                                  ]
                                                  (lam thunk Unit LT)
                                                ]
                                                Unit
                                              ]
                                            )
                                          )
                                          [
                                            [
                                              [
                                                [
                                                  {
                                                    [ { Extended_match a } ds ]
                                                    (fun Unit Ordering)
                                                  }
                                                  (lam
                                                    default_arg0
                                                    a
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        fail
                                                        (abs e (type) (error e))
                                                      ]
                                                    )
                                                  )
                                                ]
                                                (lam
                                                  thunk
                                                  Unit
                                                  [
                                                    fail
                                                    (abs e (type) (error e))
                                                  ]
                                                )
                                              ]
                                              (lam
                                                thunk
                                                Unit
                                                [
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (fun Unit Ordering)
                                                        }
                                                        (lam
                                                          default_arg0
                                                          a
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              fail
                                                              (abs
                                                                e
                                                                (type)
                                                                (error e)
                                                              )
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (lam
                                                        thunk
                                                        Unit
                                                        [
                                                          fail
                                                          (abs
                                                            e (type) (error e)
                                                          )
                                                        ]
                                                      )
                                                    ]
                                                    (lam thunk Unit EQ)
                                                  ]
                                                  Unit
                                                ]
                                              )
                                            ]
                                            Unit
                                          ]
                                        )
                                      )
                                    ]
                                    Unit
                                  ]
                                )
                              )
                            ]
                            (lam
                              thunk
                              Unit
                              [
                                [
                                  [
                                    [
                                      {
                                        [ { Extended_match a } ds ]
                                        (fun Unit Ordering)
                                      }
                                      (lam default_arg0 a (lam thunk Unit LT))
                                    ]
                                    (lam thunk Unit EQ)
                                  ]
                                  (lam thunk Unit LT)
                                ]
                                Unit
                              ]
                            )
                          ]
                          (lam
                            thunk
                            Unit
                            [
                              [
                                [
                                  [
                                    {
                                      [ { Extended_match a } ds ]
                                      (fun Unit Ordering)
                                    }
                                    (lam
                                      default_arg0
                                      a
                                      (lam
                                        thunk
                                        Unit
                                        (let
                                          (nonrec)
                                          (termbind
                                            (strict)
                                            (vardecl
                                              fail
                                              (fun (all a (type) a) Ordering)
                                            )
                                            (lam
                                              ds
                                              (all a (type) a)
                                              [
                                                [
                                                  [
                                                    [
                                                      {
                                                        [
                                                          { Extended_match a }
                                                          ds
                                                        ]
                                                        (fun Unit Ordering)
                                                      }
                                                      (lam
                                                        default_arg0
                                                        a
                                                        (lam
                                                          thunk
                                                          Unit
                                                          [
                                                            [
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      {
                                                                        Extended_match
                                                                        a
                                                                      }
                                                                      ds
                                                                    ]
                                                                    (fun Unit Ordering)
                                                                  }
                                                                  (lam
                                                                    l
                                                                    a
                                                                    (lam
                                                                      thunk
                                                                      Unit
                                                                      [
                                                                        [
                                                                          [
                                                                            [
                                                                              {
                                                                                [
                                                                                  {
                                                                                    Extended_match
                                                                                    a
                                                                                  }
                                                                                  ds
                                                                                ]
                                                                                (fun Unit Ordering)
                                                                              }
                                                                              (lam
                                                                                r
                                                                                a
                                                                                (lam
                                                                                  thunk
                                                                                  Unit
                                                                                  [
                                                                                    [
                                                                                      [
                                                                                        {
                                                                                          compare
                                                                                          a
                                                                                        }
                                                                                        dOrd
                                                                                      ]
                                                                                      l
                                                                                    ]
                                                                                    r
                                                                                  ]
                                                                                )
                                                                              )
                                                                            ]
                                                                            (lam
                                                                              thunk
                                                                              Unit
                                                                              [
                                                                                fail
                                                                                (abs
                                                                                  e
                                                                                  (type)
                                                                                  (error
                                                                                    e
                                                                                  )
                                                                                )
                                                                              ]
                                                                            )
                                                                          ]
                                                                          (lam
                                                                            thunk
                                                                            Unit
                                                                            [
                                                                              fail
                                                                              (abs
                                                                                e
                                                                                (type)
                                                                                (error
                                                                                  e
                                                                                )
                                                                              )
                                                                            ]
                                                                          )
                                                                        ]
                                                                        Unit
                                                                      ]
                                                                    )
                                                                  )
                                                                ]
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    fail
                                                                    (abs
                                                                      e
                                                                      (type)
                                                                      (error e)
                                                                    )
                                                                  ]
                                                                )
                                                              ]
                                                              (lam thunk Unit GT
                                                              )
                                                            ]
                                                            Unit
                                                          ]
                                                        )
                                                      )
                                                    ]
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        [
                                                          [
                                                            [
                                                              {
                                                                [
                                                                  {
                                                                    Extended_match
                                                                    a
                                                                  }
                                                                  ds
                                                                ]
                                                                (fun Unit Ordering)
                                                              }
                                                              (lam
                                                                l
                                                                a
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    [
                                                                      [
                                                                        [
                                                                          {
                                                                            [
                                                                              {
                                                                                Extended_match
                                                                                a
                                                                              }
                                                                              ds
                                                                            ]
                                                                            (fun Unit Ordering)
                                                                          }
                                                                          (lam
                                                                            r
                                                                            a
                                                                            (lam
                                                                              thunk
                                                                              Unit
                                                                              [
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      compare
                                                                                      a
                                                                                    }
                                                                                    dOrd
                                                                                  ]
                                                                                  l
                                                                                ]
                                                                                r
                                                                              ]
                                                                            )
                                                                          )
                                                                        ]
                                                                        (lam
                                                                          thunk
                                                                          Unit
                                                                          [
                                                                            fail
                                                                            (abs
                                                                              e
                                                                              (type)
                                                                              (error
                                                                                e
                                                                              )
                                                                            )
                                                                          ]
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          fail
                                                                          (abs
                                                                            e
                                                                            (type)
                                                                            (error
                                                                              e
                                                                            )
                                                                          )
                                                                        ]
                                                                      )
                                                                    ]
                                                                    Unit
                                                                  ]
                                                                )
                                                              )
                                                            ]
                                                            (lam
                                                              thunk
                                                              Unit
                                                              [
                                                                fail
                                                                (abs
                                                                  e
                                                                  (type)
                                                                  (error e)
                                                                )
                                                              ]
                                                            )
                                                          ]
                                                          (lam thunk Unit GT)
                                                        ]
                                                        Unit
                                                      ]
                                                    )
                                                  ]
                                                  (lam thunk Unit LT)
                                                ]
                                                Unit
                                              ]
                                            )
                                          )
                                          [
                                            [
                                              [
                                                [
                                                  {
                                                    [ { Extended_match a } ds ]
                                                    (fun Unit Ordering)
                                                  }
                                                  (lam
                                                    default_arg0
                                                    a
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        fail
                                                        (abs e (type) (error e))
                                                      ]
                                                    )
                                                  )
                                                ]
                                                (lam
                                                  thunk
                                                  Unit
                                                  [
                                                    fail
                                                    (abs e (type) (error e))
                                                  ]
                                                )
                                              ]
                                              (lam
                                                thunk
                                                Unit
                                                [
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (fun Unit Ordering)
                                                        }
                                                        (lam
                                                          default_arg0
                                                          a
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              fail
                                                              (abs
                                                                e
                                                                (type)
                                                                (error e)
                                                              )
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (lam
                                                        thunk
                                                        Unit
                                                        [
                                                          fail
                                                          (abs
                                                            e (type) (error e)
                                                          )
                                                        ]
                                                      )
                                                    ]
                                                    (lam thunk Unit EQ)
                                                  ]
                                                  Unit
                                                ]
                                              )
                                            ]
                                            Unit
                                          ]
                                        )
                                      )
                                    )
                                  ]
                                  (lam thunk Unit GT)
                                ]
                                (lam
                                  thunk
                                  Unit
                                  (let
                                    (nonrec)
                                    (termbind
                                      (strict)
                                      (vardecl
                                        fail (fun (all a (type) a) Ordering)
                                      )
                                      (lam
                                        ds
                                        (all a (type) a)
                                        [
                                          [
                                            [
                                              [
                                                {
                                                  [ { Extended_match a } ds ]
                                                  (fun Unit Ordering)
                                                }
                                                (lam
                                                  default_arg0
                                                  a
                                                  (lam
                                                    thunk
                                                    Unit
                                                    [
                                                      [
                                                        [
                                                          [
                                                            {
                                                              [
                                                                {
                                                                  Extended_match
                                                                  a
                                                                }
                                                                ds
                                                              ]
                                                              (fun Unit Ordering)
                                                            }
                                                            (lam
                                                              l
                                                              a
                                                              (lam
                                                                thunk
                                                                Unit
                                                                [
                                                                  [
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            {
                                                                              Extended_match
                                                                              a
                                                                            }
                                                                            ds
                                                                          ]
                                                                          (fun Unit Ordering)
                                                                        }
                                                                        (lam
                                                                          r
                                                                          a
                                                                          (lam
                                                                            thunk
                                                                            Unit
                                                                            [
                                                                              [
                                                                                [
                                                                                  {
                                                                                    compare
                                                                                    a
                                                                                  }
                                                                                  dOrd
                                                                                ]
                                                                                l
                                                                              ]
                                                                              r
                                                                            ]
                                                                          )
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          fail
                                                                          (abs
                                                                            e
                                                                            (type)
                                                                            (error
                                                                              e
                                                                            )
                                                                          )
                                                                        ]
                                                                      )
                                                                    ]
                                                                    (lam
                                                                      thunk
                                                                      Unit
                                                                      [
                                                                        fail
                                                                        (abs
                                                                          e
                                                                          (type)
                                                                          (error
                                                                            e
                                                                          )
                                                                        )
                                                                      ]
                                                                    )
                                                                  ]
                                                                  Unit
                                                                ]
                                                              )
                                                            )
                                                          ]
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              fail
                                                              (abs
                                                                e
                                                                (type)
                                                                (error e)
                                                              )
                                                            ]
                                                          )
                                                        ]
                                                        (lam thunk Unit GT)
                                                      ]
                                                      Unit
                                                    ]
                                                  )
                                                )
                                              ]
                                              (lam
                                                thunk
                                                Unit
                                                [
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (fun Unit Ordering)
                                                        }
                                                        (lam
                                                          l
                                                          a
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              [
                                                                [
                                                                  [
                                                                    {
                                                                      [
                                                                        {
                                                                          Extended_match
                                                                          a
                                                                        }
                                                                        ds
                                                                      ]
                                                                      (fun Unit Ordering)
                                                                    }
                                                                    (lam
                                                                      r
                                                                      a
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        [
                                                                          [
                                                                            [
                                                                              {
                                                                                compare
                                                                                a
                                                                              }
                                                                              dOrd
                                                                            ]
                                                                            l
                                                                          ]
                                                                          r
                                                                        ]
                                                                      )
                                                                    )
                                                                  ]
                                                                  (lam
                                                                    thunk
                                                                    Unit
                                                                    [
                                                                      fail
                                                                      (abs
                                                                        e
                                                                        (type)
                                                                        (error e
                                                                        )
                                                                      )
                                                                    ]
                                                                  )
                                                                ]
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    fail
                                                                    (abs
                                                                      e
                                                                      (type)
                                                                      (error e)
                                                                    )
                                                                  ]
                                                                )
                                                              ]
                                                              Unit
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (lam
                                                        thunk
                                                        Unit
                                                        [
                                                          fail
                                                          (abs
                                                            e (type) (error e)
                                                          )
                                                        ]
                                                      )
                                                    ]
                                                    (lam thunk Unit GT)
                                                  ]
                                                  Unit
                                                ]
                                              )
                                            ]
                                            (lam thunk Unit LT)
                                          ]
                                          Unit
                                        ]
                                      )
                                    )
                                    [
                                      [
                                        [
                                          [
                                            {
                                              [ { Extended_match a } ds ]
                                              (fun Unit Ordering)
                                            }
                                            (lam
                                              default_arg0
                                              a
                                              (lam
                                                thunk
                                                Unit
                                                [
                                                  fail (abs e (type) (error e))
                                                ]
                                              )
                                            )
                                          ]
                                          (lam
                                            thunk
                                            Unit
                                            [ fail (abs e (type) (error e)) ]
                                          )
                                        ]
                                        (lam
                                          thunk
                                          Unit
                                          [
                                            [
                                              [
                                                [
                                                  {
                                                    [ { Extended_match a } ds ]
                                                    (fun Unit Ordering)
                                                  }
                                                  (lam
                                                    default_arg0
                                                    a
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        fail
                                                        (abs e (type) (error e))
                                                      ]
                                                    )
                                                  )
                                                ]
                                                (lam
                                                  thunk
                                                  Unit
                                                  [
                                                    fail
                                                    (abs e (type) (error e))
                                                  ]
                                                )
                                              ]
                                              (lam thunk Unit EQ)
                                            ]
                                            Unit
                                          ]
                                        )
                                      ]
                                      Unit
                                    ]
                                  )
                                )
                              ]
                              Unit
                            ]
                          )
                        ]
                        Unit
                      ]
                    )
                  )
                )
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              fOrdUpperBound0_c
              (all a (type) (fun [Ord a] (fun [UpperBound a] (fun [UpperBound a] Bool))))
            )
            (abs
              a
              (type)
              (lam
                dOrd
                [Ord a]
                (lam
                  x
                  [UpperBound a]
                  (lam
                    y
                    [UpperBound a]
                    [
                      { [ { UpperBound_match a } x ] Bool }
                      (lam
                        v
                        [Extended a]
                        (lam
                          in
                          Bool
                          [
                            { [ { UpperBound_match a } y ] Bool }
                            (lam
                              v
                              [Extended a]
                              (lam
                                in
                                Bool
                                [
                                  [
                                    [
                                      [
                                        {
                                          [
                                            Ordering_match
                                            [
                                              [ [ { hull_ccompare a } dOrd ] v ]
                                              v
                                            ]
                                          ]
                                          (fun Unit Bool)
                                        }
                                        (lam
                                          thunk
                                          Unit
                                          [
                                            [
                                              [
                                                {
                                                  [ Bool_match in ]
                                                  (fun Unit Bool)
                                                }
                                                (lam thunk Unit in)
                                              ]
                                              (lam thunk Unit True)
                                            ]
                                            Unit
                                          ]
                                        )
                                      ]
                                      (lam thunk Unit False)
                                    ]
                                    (lam thunk Unit True)
                                  ]
                                  Unit
                                ]
                              )
                            )
                          ]
                        )
                      )
                    ]
                  )
                )
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              contains
              (all a (type) (fun [Ord a] (fun [Interval a] (fun [Interval a] Bool))))
            )
            (abs
              a
              (type)
              (lam
                dOrd
                [Ord a]
                (lam
                  ds
                  [Interval a]
                  (lam
                    ds
                    [Interval a]
                    [
                      { [ { Interval_match a } ds ] Bool }
                      (lam
                        l
                        [LowerBound a]
                        (lam
                          h
                          [UpperBound a]
                          [
                            { [ { Interval_match a } ds ] Bool }
                            (lam
                              l
                              [LowerBound a]
                              (lam
                                h
                                [UpperBound a]
                                [
                                  { [ { LowerBound_match a } l ] Bool }
                                  (lam
                                    v
                                    [Extended a]
                                    (lam
                                      in
                                      Bool
                                      [
                                        { [ { LowerBound_match a } l ] Bool }
                                        (lam
                                          v
                                          [Extended a]
                                          (lam
                                            in
                                            Bool
                                            [
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        Ordering_match
                                                        [
                                                          [
                                                            [
                                                              {
                                                                hull_ccompare a
                                                              }
                                                              dOrd
                                                            ]
                                                            v
                                                          ]
                                                          v
                                                        ]
                                                      ]
                                                      (fun Unit Bool)
                                                    }
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        [
                                                          [
                                                            {
                                                              [ Bool_match in ]
                                                              (fun Unit Bool)
                                                            }
                                                            (lam
                                                              thunk
                                                              Unit
                                                              [
                                                                [
                                                                  [
                                                                    {
                                                                      [
                                                                        Bool_match
                                                                        in
                                                                      ]
                                                                      (fun Unit Bool)
                                                                    }
                                                                    (lam
                                                                      thunk
                                                                      Unit
                                                                      [
                                                                        [
                                                                          [
                                                                            {
                                                                              fOrdUpperBound0_c
                                                                              a
                                                                            }
                                                                            dOrd
                                                                          ]
                                                                          h
                                                                        ]
                                                                        h
                                                                      ]
                                                                    )
                                                                  ]
                                                                  (lam
                                                                    thunk
                                                                    Unit
                                                                    False
                                                                  )
                                                                ]
                                                                Unit
                                                              ]
                                                            )
                                                          ]
                                                          (lam
                                                            thunk
                                                            Unit
                                                            [
                                                              [
                                                                [
                                                                  {
                                                                    fOrdUpperBound0_c
                                                                    a
                                                                  }
                                                                  dOrd
                                                                ]
                                                                h
                                                              ]
                                                              h
                                                            ]
                                                          )
                                                        ]
                                                        Unit
                                                      ]
                                                    )
                                                  ]
                                                  (lam thunk Unit False)
                                                ]
                                                (lam
                                                  thunk
                                                  Unit
                                                  [
                                                    [
                                                      [
                                                        { fOrdUpperBound0_c a }
                                                        dOrd
                                                      ]
                                                      h
                                                    ]
                                                    h
                                                  ]
                                                )
                                              ]
                                              Unit
                                            ]
                                          )
                                        )
                                      ]
                                    )
                                  )
                                ]
                              )
                            )
                          ]
                        )
                      )
                    ]
                  )
                )
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Monoid (fun (type) (type)))
              (tyvardecl a (type))
              Monoid_match
              (vardecl
                CConsMonoid
                (fun [(lam a (type) (fun a (fun a a))) a] (fun a [Monoid a]))
              )
            )
          )
          (let
            (rec)
            (termbind
              (nonstrict)
              (vardecl
                fFoldableNil_cfoldMap
                (all m (type) (all a (type) (fun [Monoid m] (fun (fun a m) (fun [List a] m)))))
              )
              (abs
                m
                (type)
                (abs
                  a
                  (type)
                  (lam
                    dMonoid
                    [Monoid m]
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl dSemigroup [(lam a (type) (fun a (fun a a))) m]
                        )
                        [
                          {
                            [ { Monoid_match m } dMonoid ]
                            [(lam a (type) (fun a (fun a a))) m]
                          }
                          (lam
                            v [(lam a (type) (fun a (fun a a))) m] (lam v m v)
                          )
                        ]
                      )
                      (lam
                        ds
                        (fun a m)
                        (lam
                          ds
                          [List a]
                          [
                            [
                              [
                                { [ { Nil_match a } ds ] (fun Unit m) }
                                (lam
                                  thunk
                                  Unit
                                  [
                                    { [ { Monoid_match m } dMonoid ] m }
                                    (lam
                                      v
                                      [(lam a (type) (fun a (fun a a))) m]
                                      (lam v m v)
                                    )
                                  ]
                                )
                              ]
                              (lam
                                x
                                a
                                (lam
                                  xs
                                  [List a]
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      [ dSemigroup [ ds x ] ]
                                      [
                                        [
                                          [
                                            { { fFoldableNil_cfoldMap m } a }
                                            dMonoid
                                          ]
                                          ds
                                        ]
                                        xs
                                      ]
                                    ]
                                  )
                                )
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                    )
                  )
                )
              )
            )
            (let
              (nonrec)
              (termbind
                (strict)
                (vardecl txSignedBy (fun TxInfo (fun (con bytestring) Bool)))
                (lam
                  ds
                  TxInfo
                  (lam
                    k
                    (con bytestring)
                    [
                      { [ TxInfo_match ds ] Bool }
                      (lam
                        ds
                        [List TxInInfo]
                        (lam
                          ds
                          [List TxOut]
                          (lam
                            ds
                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                            (lam
                              ds
                              [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                              (lam
                                ds
                                [List DCert]
                                (lam
                                  ds
                                  [List [[Tuple2 StakingCredential] (con integer)]]
                                  (lam
                                    ds
                                    [Interval (con integer)]
                                    (lam
                                      ds
                                      [List (con bytestring)]
                                      (lam
                                        ds
                                        [List [[Tuple2 (con bytestring)] Data]]
                                        (lam
                                          ds
                                          (con bytestring)
                                          [
                                            [
                                              [
                                                {
                                                  [
                                                    {
                                                      Maybe_match
                                                      (con bytestring)
                                                    }
                                                    [
                                                      [
                                                        [
                                                          {
                                                            {
                                                              fFoldableNil_cfoldMap
                                                              [(lam a (type) [Maybe a]) (con bytestring)]
                                                            }
                                                            (con bytestring)
                                                          }
                                                          [
                                                            [
                                                              {
                                                                CConsMonoid
                                                                [(lam a (type) [Maybe a]) (con bytestring)]
                                                              }
                                                              (lam
                                                                ds
                                                                [(lam a (type) [Maybe a]) (con bytestring)]
                                                                (lam
                                                                  b
                                                                  [(lam a (type) [Maybe a]) (con bytestring)]
                                                                  [
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            {
                                                                              Maybe_match
                                                                              (con bytestring)
                                                                            }
                                                                            ds
                                                                          ]
                                                                          (fun Unit [(lam a (type) [Maybe a]) (con bytestring)])
                                                                        }
                                                                        (lam
                                                                          ipv
                                                                          (con bytestring)
                                                                          (lam
                                                                            thunk
                                                                            Unit
                                                                            ds
                                                                          )
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        thunk
                                                                        Unit
                                                                        b
                                                                      )
                                                                    ]
                                                                    Unit
                                                                  ]
                                                                )
                                                              )
                                                            ]
                                                            {
                                                              Nothing
                                                              (con bytestring)
                                                            }
                                                          ]
                                                        ]
                                                        (lam
                                                          x
                                                          (con bytestring)
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    Bool_match
                                                                    (let
                                                                      (nonrec)
                                                                      (termbind
                                                                        (strict)
                                                                        (vardecl
                                                                          b
                                                                          (con bool)
                                                                        )
                                                                        [
                                                                          [
                                                                            (builtin
                                                                              equalsByteString
                                                                            )
                                                                            k
                                                                          ]
                                                                          x
                                                                        ]
                                                                      )
                                                                      [
                                                                        [
                                                                          [
                                                                            {
                                                                              (builtin
                                                                                ifThenElse
                                                                              )
                                                                              Bool
                                                                            }
                                                                            b
                                                                          ]
                                                                          True
                                                                        ]
                                                                        False
                                                                      ]
                                                                    )
                                                                  ]
                                                                  (fun Unit [Maybe (con bytestring)])
                                                                }
                                                                (lam
                                                                  thunk
                                                                  Unit
                                                                  [
                                                                    {
                                                                      Just
                                                                      (con bytestring)
                                                                    }
                                                                    x
                                                                  ]
                                                                )
                                                              ]
                                                              (lam
                                                                thunk
                                                                Unit
                                                                {
                                                                  Nothing
                                                                  (con bytestring)
                                                                }
                                                              )
                                                            ]
                                                            Unit
                                                          ]
                                                        )
                                                      ]
                                                      ds
                                                    ]
                                                  ]
                                                  (fun Unit Bool)
                                                }
                                                (lam
                                                  ds
                                                  (con bytestring)
                                                  (lam thunk Unit True)
                                                )
                                              ]
                                              (lam thunk Unit False)
                                            ]
                                            Unit
                                          ]
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ]
                  )
                )
              )
              (lam
                c
                Campaign
                (lam
                  con
                  (con bytestring)
                  (lam
                    act
                    CampaignAction
                    (lam
                      ds
                      ScriptContext
                      [
                        { [ ScriptContext_match ds ] Bool }
                        (lam
                          ds
                          TxInfo
                          (lam
                            ds
                            ScriptPurpose
                            [
                              [
                                [
                                  {
                                    [ CampaignAction_match act ] (fun Unit Bool)
                                  }
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      [
                                        [
                                          {
                                            [
                                              Bool_match
                                              [
                                                [
                                                  [
                                                    { contains (con integer) }
                                                    fOrdPOSIXTime
                                                  ]
                                                  [
                                                    [
                                                      { Interval (con integer) }
                                                      [
                                                        [
                                                          {
                                                            LowerBound
                                                            (con integer)
                                                          }
                                                          [
                                                            {
                                                              Finite
                                                              (con integer)
                                                            }
                                                            [
                                                              {
                                                                [
                                                                  Campaign_match
                                                                  c
                                                                ]
                                                                (con integer)
                                                              }
                                                              (lam
                                                                ds
                                                                (con integer)
                                                                (lam
                                                                  ds
                                                                  (con integer)
                                                                  (lam
                                                                    ds
                                                                    (con bytestring)
                                                                    ds
                                                                  )
                                                                )
                                                              )
                                                            ]
                                                          ]
                                                        ]
                                                        True
                                                      ]
                                                    ]
                                                    [
                                                      [
                                                        {
                                                          UpperBound
                                                          (con integer)
                                                        }
                                                        [
                                                          {
                                                            Finite (con integer)
                                                          }
                                                          [
                                                            {
                                                              [
                                                                Campaign_match c
                                                              ]
                                                              (con integer)
                                                            }
                                                            (lam
                                                              ds
                                                              (con integer)
                                                              (lam
                                                                ds
                                                                (con integer)
                                                                (lam
                                                                  ds
                                                                  (con bytestring)
                                                                  ds
                                                                )
                                                              )
                                                            )
                                                          ]
                                                        ]
                                                      ]
                                                      True
                                                    ]
                                                  ]
                                                ]
                                                [
                                                  {
                                                    [ TxInfo_match ds ]
                                                    [Interval (con integer)]
                                                  }
                                                  (lam
                                                    ds
                                                    [List TxInInfo]
                                                    (lam
                                                      ds
                                                      [List TxOut]
                                                      (lam
                                                        ds
                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                        (lam
                                                          ds
                                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                          (lam
                                                            ds
                                                            [List DCert]
                                                            (lam
                                                              ds
                                                              [List [[Tuple2 StakingCredential] (con integer)]]
                                                              (lam
                                                                ds
                                                                [Interval (con integer)]
                                                                (lam
                                                                  ds
                                                                  [List (con bytestring)]
                                                                  (lam
                                                                    ds
                                                                    [List [[Tuple2 (con bytestring)] Data]]
                                                                    (lam
                                                                      ds
                                                                      (con bytestring)
                                                                      ds
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                ]
                                              ]
                                            ]
                                            (fun Unit Bool)
                                          }
                                          (lam
                                            thunk
                                            Unit
                                            [
                                              [ txSignedBy ds ]
                                              [
                                                {
                                                  [ Campaign_match c ]
                                                  (con bytestring)
                                                }
                                                (lam
                                                  ds
                                                  (con integer)
                                                  (lam
                                                    ds
                                                    (con integer)
                                                    (lam ds (con bytestring) ds)
                                                  )
                                                )
                                              ]
                                            ]
                                          )
                                        ]
                                        (lam thunk Unit False)
                                      ]
                                      Unit
                                    ]
                                  )
                                ]
                                (lam
                                  thunk
                                  Unit
                                  [
                                    [
                                      [
                                        {
                                          [
                                            Bool_match
                                            [
                                              [
                                                [
                                                  { contains (con integer) }
                                                  fOrdPOSIXTime
                                                ]
                                                [
                                                  [
                                                    { Interval (con integer) }
                                                    [
                                                      [
                                                        {
                                                          LowerBound
                                                          (con integer)
                                                        }
                                                        [
                                                          {
                                                            Finite (con integer)
                                                          }
                                                          [
                                                            {
                                                              [
                                                                Campaign_match c
                                                              ]
                                                              (con integer)
                                                            }
                                                            (lam
                                                              ds
                                                              (con integer)
                                                              (lam
                                                                ds
                                                                (con integer)
                                                                (lam
                                                                  ds
                                                                  (con bytestring)
                                                                  ds
                                                                )
                                                              )
                                                            )
                                                          ]
                                                        ]
                                                      ]
                                                      True
                                                    ]
                                                  ]
                                                  [
                                                    [
                                                      {
                                                        UpperBound (con integer)
                                                      }
                                                      { PosInf (con integer) }
                                                    ]
                                                    True
                                                  ]
                                                ]
                                              ]
                                              [
                                                {
                                                  [ TxInfo_match ds ]
                                                  [Interval (con integer)]
                                                }
                                                (lam
                                                  ds
                                                  [List TxInInfo]
                                                  (lam
                                                    ds
                                                    [List TxOut]
                                                    (lam
                                                      ds
                                                      [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                      (lam
                                                        ds
                                                        [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                                        (lam
                                                          ds
                                                          [List DCert]
                                                          (lam
                                                            ds
                                                            [List [[Tuple2 StakingCredential] (con integer)]]
                                                            (lam
                                                              ds
                                                              [Interval (con integer)]
                                                              (lam
                                                                ds
                                                                [List (con bytestring)]
                                                                (lam
                                                                  ds
                                                                  [List [[Tuple2 (con bytestring)] Data]]
                                                                  (lam
                                                                    ds
                                                                    (con bytestring)
                                                                    ds
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              ]
                                            ]
                                          ]
                                          (fun Unit Bool)
                                        }
                                        (lam
                                          thunk Unit [ [ txSignedBy ds ] con ]
                                        )
                                      ]
                                      (lam thunk Unit False)
                                    ]
                                    Unit
                                  ]
                                )
                              ]
                              Unit
                            ]
                          )
                        )
                      ]
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)