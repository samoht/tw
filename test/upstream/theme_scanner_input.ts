// Not upstream Tailwind: a hand-written stand-in for the shapes Prettier
// produces in utilities.test.ts, so the @theme scanner is pinned on values
// that do not fit on one line.
test('multiline-theme', async () => {
  expect(
    await run(
      ['font-sans', 'shadow-wrapped'],
      css`
        @theme {
          --spacing-*: initial;
          --font-sans:
            ui-sans-serif, system-ui, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji',
            'Segoe UI Symbol', 'Noto Color Emoji';
          --single-line: 1rem;
          /* a comment; with a semicolon and a } brace */
          --quoted: 'a;b}c';
          --shadow-wrapped:
            0 1px 2px 0 rgb(0 0 0 / 0.05),
            0 1px 3px 0
              rgb(
                0 0 0 / 0.1
              );
          --after-the-nesting: 2px;
        }
        @tailwind utilities;
      `,
    ),
  ).toMatchInlineSnapshot(`
    "
    .font-sans {
      font-family: var(--font-sans);
    }
    "
  `)
})
