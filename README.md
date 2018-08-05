# TF Language

for programming language seminar.

## Sample

```bash
$ echo "let i = 20 in if i - 1 then 3 else 4 * i" | stack exec language-tf
Success to parse:
(let i = 20 in (if (i - 1) then 3 else (4 * i)))
```

```bash
$ echo "let i in 4 *" | stack exec language-tf
Failed to parse:
(interactive):1:7: error: expected: "="
let i in 4 *
      ^
```
