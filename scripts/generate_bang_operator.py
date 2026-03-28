import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

DOC_NAME_ALIASES = {
    "con": "concat",
    "logtwo": "log2",
}

SYNTAX_KIND_BY_NAME = {
    "add": "XAdd",
    "and": "XAnd",
    "cast": "XCast",
    "concat": "XCon",
    "cond": "XCond",
    "dag": "XDag",
    "div": "XDiv",
    "empty": "XEmpty",
    "eq": "XEq",
    "exists": "XExists",
    "filter": "XFilter",
    "find": "XFind",
    "foldl": "XFoldl",
    "foreach": "XForEach",
    "ge": "XGe",
    "getdagarg": "XGetDagArg",
    "getdagname": "XGetDagName",
    "getdagop": "XGetDagOp",
    "gt": "XGt",
    "head": "XHead",
    "if": "XIf",
    "initialized": "XInitialized",
    "instances": "XInstances",
    "interleave": "XInterleave",
    "isa": "XIsA",
    "le": "XLe",
    "listconcat": "XListConcat",
    "listflatten": "XListFlatten",
    "listremove": "XListRemove",
    "listsplat": "XListSplat",
    "log2": "XLog2",
    "lt": "XLt",
    "match": "XMatch",
    "mul": "XMul",
    "ne": "XNe",
    "not": "XNot",
    "or": "XOr",
    "range": "XRange",
    "repr": "XRepr",
    "setdagarg": "XSetDagArg",
    "setdagname": "XSetDagName",
    "setdagop": "XSetDagOp",
    "shl": "XShl",
    "size": "XSize",
    "sra": "XSra",
    "srl": "XSrl",
    "strconcat": "XStrConcat",
    "sub": "XSub",
    "subst": "XSubst",
    "substr": "XSubstr",
    "tail": "XTail",
    "tolower": "XToLower",
    "toupper": "XToUpper",
    "xor": "XXor",
}


@dataclass
class ParsedOperator:
    doc_name: str
    canonical_name: str
    signature: str
    parsed_signature: "ParsedSignature"
    documentation: str
    needs_type_annotation: bool
    min_args: int
    max_args: int | None


@dataclass
class SignatureParam:
    name: str
    optional: bool
    label_start: int
    label_end: int


@dataclass
class ParsedSignature:
    text: str
    params: list[SignatureParam]
    min_args: int
    max_args: int | None
    needs_type_annotation: bool


def warn(message: str) -> None:
    print(f"[WARN] {message}", file=sys.stderr)


def extract_appendix_a(progref: str) -> str:
    start_match = re.search(
        r"^Appendix A: Bang Operators\n=+\n",
        progref,
        re.MULTILINE,
    )
    assert start_match is not None

    end_match = re.search(
        r"^Appendix B: Paste Operator Examples\n=+\n",
        progref,
        re.MULTILINE,
    )
    assert end_match is not None

    return progref[start_match.start() : end_match.start()]


def normalize_text(text: str) -> str:
    text = text.replace("\\", "")
    text = re.sub(r"``([^`]+)``", r"`\1`", text)
    return text


def strip_signature_markup(text: str) -> str:
    text = text.replace("`", "")
    text = text.replace("*", "")
    text = re.sub(r"\s+", " ", text).strip()
    text = re.sub(r"\s*\(\s*", "(", text)
    text = re.sub(r"\s*\)\s*", ")", text)
    text = re.sub(r"\s*<\s*", "<", text)
    text = re.sub(r"\s*>\s*", ">", text)
    text = re.sub(r"\s*\[\s*", "[", text)
    text = re.sub(r"\s*\]\s*", "]", text)
    text = re.sub(r"\s*:\s*", ":", text)
    text = re.sub(r"\s*,\s*", ", ", text)
    return text


def parse_appendix_a_operators(appendix_a: str) -> list[tuple[str, list[str]]]:
    lines = appendix_a.splitlines()
    signature_pattern = re.compile(r"^``!([a-z0-9]+).+``$")

    entries: list[tuple[str, list[str]]] = []
    current_name: str | None = None
    current_block: list[str] = []

    for line in lines:
        signature_match = signature_pattern.match(line)
        if signature_match:
            if current_name is not None and current_block:
                entries.append((current_name, current_block))

            current_name = signature_match.group(1)
            current_block = [line.strip()]
            continue

        if current_name is None:
            continue

        if line == "" or line.startswith(" "):
            current_block.append(line)
            continue

        entries.append((current_name, current_block))
        current_name = None
        current_block = []

    if current_name is not None and current_block:
        entries.append((current_name, current_block))

    return entries


def parse_signature(signature: str) -> ParsedSignature:
    def optional_depth_at(text: str, pos: int) -> int:
        return text[:pos].count("[") - text[:pos].count("]")

    overload_texts = [part.strip() for part in signature.split("--or--") if part.strip()]
    primary_text_markup = overload_texts[0] if overload_texts else signature
    primary_text_plain = strip_signature_markup(primary_text_markup)

    open_paren = primary_text_markup.find("(")
    close_paren = primary_text_markup.rfind(")")
    open_paren_plain = primary_text_plain.find("(")
    args_spec = ""
    args_spec_plain = ""
    has_variadic = False
    if open_paren >= 0 and close_paren >= open_paren and open_paren_plain >= 0:
        args_spec = primary_text_markup[open_paren + 1 : close_paren]
        close_paren_plain = primary_text_plain.rfind(")")
        if close_paren_plain >= open_paren_plain:
            args_spec_plain = primary_text_plain[open_paren_plain + 1 : close_paren_plain]
        has_variadic = "..." in args_spec

    params: list[SignatureParam] = []
    if args_spec:
        search_pos = 0
        for match in re.finditer(r"\*([^*]+)\*", args_spec):
            name = match.group(1).strip()
            if not name:
                continue

            optional = optional_depth_at(args_spec, match.start()) > 0
            plain_index = args_spec_plain.find(name, search_pos)
            if plain_index < 0:
                continue
            label_start = open_paren_plain + 1 + plain_index
            label_end = label_start + len(name)
            search_pos = plain_index + len(name)
            params.append(
                SignatureParam(
                    name=name,
                    optional=optional,
                    label_start=label_start,
                    label_end=label_end,
                )
            )

    min_args = sum(1 for param in params if not param.optional)
    max_args: int | None = None if has_variadic else len(params)

    needs_type_annotation = any(re.search(r"`![a-z0-9]+<", alt) for alt in overload_texts)
    return ParsedSignature(
        text=primary_text_plain,
        params=params,
        min_args=min_args,
        max_args=max_args,
        needs_type_annotation=needs_type_annotation,
    )


def build_documentation_from_lines(block_lines: list[str]) -> tuple[str, str]:
    signature = normalize_text(block_lines[0].strip())

    body_lines: list[str] = []
    for raw_line in block_lines[1:]:
        line = raw_line
        if line.startswith("    "):
            line = line[4:]
        body_lines.append(normalize_text(line.rstrip()))

    non_empty = [line for line in body_lines if line.strip()]
    if non_empty:
        common_indent = min(len(line) - len(line.lstrip(" ")) for line in non_empty)
        if common_indent > 0:
            body_lines = [
                line[common_indent:] if len(line) >= common_indent else line
                for line in body_lines
            ]

    converted_lines: list[str] = []
    idx = 0
    while idx < len(body_lines):
        line = body_lines[idx]
        stripped = line.rstrip()
        if stripped.endswith("::"):
            code_start = idx + 1
            while code_start < len(body_lines) and body_lines[code_start] == "":
                code_start += 1

            if code_start < len(body_lines):
                code_indent = len(body_lines[code_start]) - len(
                    body_lines[code_start].lstrip(" ")
                )
                code_end = code_start
                code_lines: list[str] = []
                while code_end < len(body_lines):
                    candidate = body_lines[code_end]
                    if candidate == "":
                        code_lines.append("")
                        code_end += 1
                        continue
                    candidate_indent = len(candidate) - len(candidate.lstrip(" "))
                    if candidate_indent < code_indent:
                        break
                    code_lines.append(candidate[code_indent:])
                    code_end += 1

                if code_lines:
                    while code_lines and code_lines[-1] == "":
                        code_lines.pop()
                    converted_lines.append(stripped[:-1])
                    converted_lines.append("")
                    converted_lines.append("```")
                    converted_lines.extend(code_lines)
                    converted_lines.append("```")
                    idx = code_end
                    continue

        converted_lines.append(line)
        idx += 1

    body_lines = converted_lines

    while body_lines and body_lines[0] == "":
        body_lines.pop(0)
    while body_lines and body_lines[-1] == "":
        body_lines.pop()

    documentation = "\n".join(body_lines)
    return signature, documentation


def parse_operators_from_progref(progref_text: str) -> list[ParsedOperator]:
    appendix = extract_appendix_a(progref_text)
    entries = parse_appendix_a_operators(appendix)

    parsed: list[ParsedOperator] = []
    for doc_name, block_lines in entries:
        canonical_name = DOC_NAME_ALIASES.get(doc_name, doc_name)
        signature, documentation = build_documentation_from_lines(block_lines)
        parsed_signature = parse_signature(signature)

        parsed.append(
            ParsedOperator(
                doc_name=doc_name,
                canonical_name=canonical_name,
                signature=parsed_signature.text,
                parsed_signature=parsed_signature,
                documentation=documentation,
                needs_type_annotation=parsed_signature.needs_type_annotation,
                min_args=parsed_signature.min_args,
                max_args=parsed_signature.max_args,
            )
        )

    return parsed


def rust_string_literal(value: str) -> str:
    if '"#' not in value:
        return f'r#"{value}"#'
    escaped = value.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def build_rust_source(
    parsed_operators: list[ParsedOperator],
    syntax_kind_by_name: dict[str, str],
) -> str:
    TEMPLATE = """// DO NOT EDIT THIS FILE MANUALLY. IT IS GENERATED BY scripts/generate_bang_operator.py
use syntax::syntax_kind::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BangOperatorMetadata {
    pub name: &'static str,
    pub signature: &'static str,
    pub needs_type_annotation: bool,
    pub min_args: usize,
    pub max_args: Option<usize>,
    pub syntax_kind: SyntaxKind,
    pub signature_params: &'static [BangSignatureParamMetadata],
    pub documentation: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BangSignatureParamMetadata {
    pub name: &'static str,
    pub optional: bool,
    pub label_start: usize,
    pub label_end: usize,
}

pub fn get_metadata_for_syntax_kind(kind: SyntaxKind) -> Option<&'static BangOperatorMetadata> {
    OPS.iter().find(|meta| meta.syntax_kind == kind)
}

pub const OPS: &[BangOperatorMetadata] = &[
%%OPS%%
];
  """

    ops: list[str] = []
    for operator in parsed_operators:
        syntax_kind = syntax_kind_by_name.get(operator.canonical_name)
        assert syntax_kind is not None

        max_args_literal = (
            f"Some({operator.max_args})" if operator.max_args is not None else "None"
        )

        params_literal_parts: list[str] = []
        for param in operator.parsed_signature.params:
            params_literal_parts.append(
                "BangSignatureParamMetadata { "
                f'name: "{param.name}", '
                f"optional: {'true' if param.optional else 'false'}, "
                f"label_start: {param.label_start}, "
                f"label_end: {param.label_end} "
                "}"
            )

        params_literal = f"&[{', '.join(params_literal_parts)}]"

        ops.append(f"""    BangOperatorMetadata {{
        name: "{operator.canonical_name}",
        signature: {rust_string_literal(operator.signature)},
        needs_type_annotation: {"true" if operator.needs_type_annotation else "false"},
        min_args: {operator.min_args},
        max_args: {max_args_literal},
        syntax_kind: SyntaxKind::{syntax_kind},
        signature_params: {params_literal},
        documentation: {rust_string_literal(operator.documentation)},
    }},""")

    return TEMPLATE.replace("%%OPS%%", "\n".join(ops))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "input",
        type=Path,
        help="Path to LLVM ProgRef.rst",
    )
    args = parser.parse_args()

    progref_text = args.input.read_text()
    parsed_operators = parse_operators_from_progref(progref_text)
    generated_rust = build_rust_source(parsed_operators, SYNTAX_KIND_BY_NAME)
    print(generated_rust)


if __name__ == "__main__":
    main()
