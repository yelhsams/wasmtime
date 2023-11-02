use veri_ir::{TermSignature, Type};

use std::{collections::HashMap, vec};

pub fn isle_inst_types() -> HashMap<&'static str, Vec<TermSignature>> {
    let bv_types_8_to_64: Vec<Type> = vec![
        Type::BitVector(Some(8)),
        Type::BitVector(Some(16)),
        Type::BitVector(Some(32)),
        Type::BitVector(Some(64)),
    ];

    let bv_unary_8_to_64: Vec<TermSignature> = bv_types_8_to_64
        .iter()
        .copied()
        .map(|x| TermSignature {
            args: vec![x.clone()],
            ret: x.clone(),
            canonical_type: Some(x),
        })
        .collect();

    let bv_binary_8_to_64: Vec<TermSignature> = bv_types_8_to_64
        .iter()
        .copied()
        .map(|x| TermSignature {
            args: vec![x.clone(), x.clone()],
            ret: x.clone(),
            canonical_type: Some(x),
        })
        .collect();

    let bv_ternary_8_to_64: Vec<TermSignature> = bv_types_8_to_64
        .iter()
        .copied()
        .map(|x| TermSignature {
            args: vec![x.clone(), x.clone(), x.clone()],
            ret: x.clone(),
            canonical_type: Some(x),
        })
        .collect();

    let mut widths = HashMap::new();

    // Simple unary
    widths.insert("ineg", bv_unary_8_to_64.clone());
    widths.insert("iabs", bv_unary_8_to_64.clone());
    widths.insert("bnot", bv_unary_8_to_64.clone());
    widths.insert("cls", bv_unary_8_to_64.clone());
    widths.insert("clz", bv_unary_8_to_64.clone());
    widths.insert("ctz", bv_unary_8_to_64.clone());
    widths.insert("popcnt", bv_unary_8_to_64.clone());

    widths.insert(
        "operand_size",
        bv_types_8_to_64
            .iter()
            .copied()
            .map(|t| TermSignature {
                args: vec![Type::Int],
                ret: Type::Int,
                canonical_type: Some(t),
            })
            .collect(),
    );

    // (decl output_reg (Reg) InstOutput)
    widths.insert(
        "output_reg",
        bv_types_8_to_64
            .iter()
            .copied()
            .map(|t| TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: t.clone(),
                canonical_type: Some(t),
            })
            .collect(),
    );

    // (decl imm (Type u64) Reg)
    widths.insert(
        "imm",
        bv_types_8_to_64
            .iter()
            .copied()
            .map(|t| TermSignature {
                args: vec![Type::Int, Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(t.clone()),
            })
            .collect(),
    );

    // Unary with variable return width
    let extends = vec![
        TermSignature {
            args: vec![Type::BitVector(Some(8))],
            ret: Type::BitVector(Some(8)),
            canonical_type: Some(Type::BitVector(Some(8))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(8))],
            ret: Type::BitVector(Some(16)),
            canonical_type: Some(Type::BitVector(Some(8))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(8))],
            ret: Type::BitVector(Some(32)),
            canonical_type: Some(Type::BitVector(Some(8))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(8))],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(8))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(16))],
            ret: Type::BitVector(Some(16)),
            canonical_type: Some(Type::BitVector(Some(16))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(16))],
            ret: Type::BitVector(Some(32)),
            canonical_type: Some(Type::BitVector(Some(16))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(16))],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(16))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(32))],
            ret: Type::BitVector(Some(32)),
            canonical_type: Some(Type::BitVector(Some(32))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(32))],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(32))),
        },
        TermSignature {
            args: vec![Type::BitVector(Some(64))],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        },
    ];
    widths.insert("sextend", extends.clone());
    widths.insert("uextend", extends.clone());

    // Binary
    widths.insert("iadd", bv_binary_8_to_64.clone());
    widths.insert("isub", bv_binary_8_to_64.clone());
    widths.insert("imul", bv_binary_8_to_64.clone());
    widths.insert("band", bv_binary_8_to_64.clone());
    widths.insert("band_not", bv_binary_8_to_64.clone());
    widths.insert("bor", bv_binary_8_to_64.clone());
    widths.insert("bxor", bv_binary_8_to_64.clone());
    widths.insert("ushr", bv_binary_8_to_64.clone());
    widths.insert("sshr", bv_binary_8_to_64.clone());
    widths.insert("ishl", bv_binary_8_to_64.clone());
    widths.insert("udiv", bv_binary_8_to_64.clone());
    widths.insert("sdiv", bv_binary_8_to_64.clone());
    widths.insert("srem", bv_binary_8_to_64.clone());
    widths.insert("urem", bv_binary_8_to_64.clone());
    widths.insert("smin", bv_binary_8_to_64.clone());
    widths.insert("umin", bv_binary_8_to_64.clone());
    widths.insert("smax", bv_binary_8_to_64.clone());
    widths.insert("umax", bv_binary_8_to_64.clone());

    // x86 binary
    widths.insert(
        "amode_add",
        vec![
            TermSignature {
                args: vec![Type::BitVector(Some(64)), Type::BitVector(Some(32))],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64)), Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    // (decl to_amode_add (MemFlags Value Value Offset32) Amode)
    widths.insert(
        "to_amode_add",
        vec![TermSignature {
            args: vec![
                Type::BitVector(Some(8)),
                Type::BitVector(Some(64)),
                Type::BitVector(Some(64)),
                Type::BitVector(Some(32)),
            ],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        }],
    );

    // (decl amode_imm_reg (MemFlags Value Offset32) Amode)
    widths.insert(
        "amode_imm_reg",
        vec![TermSignature {
            args: vec![
                Type::BitVector(Some(8)),
                Type::BitVector(Some(64)),
                Type::BitVector(Some(32)),
            ],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        }],
    );

    // (decl amode_imm_reg_reg_shift (MemFlags Value Value Offset32) Amode)
    widths.insert(
        "amode_imm_reg_reg_shift",
        vec![TermSignature {
            args: vec![
                Type::BitVector(Some(8)),
                Type::BitVector(Some(64)),
                Type::BitVector(Some(64)),
                Type::BitVector(Some(32)),
            ],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        }],
    );

    // Binary with possibly differing widths
    widths.insert("rotl", bv_binary_8_to_64.clone());
    widths.insert("rotr", bv_binary_8_to_64.clone());

    // Ternary
    widths.insert("bitselect", bv_ternary_8_to_64.clone());

    widths.insert(
        "iconst",
        vec![
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(16)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(32)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    widths.insert(
        "null",
        vec![
            TermSignature {
                args: vec![],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![],
                ret: Type::BitVector(Some(16)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![],
                ret: Type::BitVector(Some(32)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    widths.insert(
        "icmp",
        vec![
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(16)),
                    Type::BitVector(Some(16)),
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(32)),
                    Type::BitVector(Some(32)),
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(64)),
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    widths.insert(
        "lower_icmp_into_reg",
        vec![
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::Int,
                    Type::Int,
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(16)),
                    Type::BitVector(Some(16)),
                    Type::Int,
                    Type::Int,
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(32)),
                    Type::BitVector(Some(32)),
                    Type::Int,
                    Type::Int,
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                    Type::Int,
                ],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    widths.insert(
        "lower_icmp",
        vec![
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(16)),
                    Type::BitVector(Some(16)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(32)),
                    Type::BitVector(Some(32)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    // (decl lower_icmp_const (IntCC Value u64 Type) FlagsAndCC)
    widths.insert(
        "lower_icmp_const",
        vec![
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(16)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(32)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(64)),
                    Type::Int,
                ],
                ret: Type::BitVector(Some(12)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    // Intermediate terms
    // (decl small_rotr (Type Reg Reg) Reg)
    widths.insert(
        "small_rotr",
        vec![TermSignature {
            args: vec![
                Type::Int,
                Type::BitVector(Some(64)),
                Type::BitVector(Some(64)),
            ],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        }],
    );

    // (decl small_rotr_imm (Type Reg ImmShift) Reg)
    widths.insert(
        "small_rotr_imm",
        vec![TermSignature {
            args: vec![
                Type::Int,
                Type::BitVector(Some(64)),
                Type::BitVector(Some(6)),
            ],
            ret: Type::BitVector(Some(64)),
            canonical_type: Some(Type::BitVector(Some(64))),
        }],
    );

    // (decl do_shift (ALUOp Type Reg Value) Reg)
    widths.insert(
        "do_shift",
        vec![
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::Int,
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(8)),
                ],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::Int,
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(16)),
                ],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::Int,
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(32)),
                ],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![
                    Type::BitVector(Some(8)),
                    Type::Int,
                    Type::BitVector(Some(64)),
                    Type::BitVector(Some(64)),
                ],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    //  (decl pure imm12_from_negated_value (Value) Imm12)
    widths.insert(
        "imm12_from_negated_value",
        vec![
            TermSignature {
                args: vec![Type::BitVector(Some(8))],
                ret: Type::BitVector(Some(24)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(16))],
                ret: Type::BitVector(Some(24)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(32))],
                ret: Type::BitVector(Some(24)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(24)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    widths.insert(
        "simplify",
        vec![
            TermSignature {
                args: vec![Type::BitVector(Some(8))],
                ret: Type::BitVector(Some(8)),
                canonical_type: Some(Type::BitVector(Some(8))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(16))],
                ret: Type::BitVector(Some(16)),
                canonical_type: Some(Type::BitVector(Some(16))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(32))],
                ret: Type::BitVector(Some(32)),
                canonical_type: Some(Type::BitVector(Some(32))),
            },
            TermSignature {
                args: vec![Type::BitVector(Some(64))],
                ret: Type::BitVector(Some(64)),
                canonical_type: Some(Type::BitVector(Some(64))),
            },
        ],
    );

    return widths;
}
