import tseslint from "typescript-eslint";
const typescriptEslint = tseslint.plugin;
const typescriptParser = tseslint.parser;

/** @type {import("eslint").Linter.Config[]} */
export default [
    {
        ignores: ["**/node_modules", "**/out", "**/*.d.ts"],
    },
    ...tseslint.configs.recommendedTypeChecked,
    ...tseslint.configs.stylisticTypeChecked,
    {
        languageOptions: {
            parser: typescriptParser,
            parserOptions: {
                projectService: true,
                tsconfigRootDir: import.meta.dirname,
                ecmaVersion: 6,
                sourceType: "module",
            },
        },
        plugins: {
            "@typescript-eslint": typescriptEslint,
        },
        rules: {
            "@typescript-eslint/naming-convention": "warn",
            "@/curly": "warn",
            "@/eqeqeq": "warn",
            "no-throw-literal": "warn",
            "@/semi": "off",
        },
    },
];
