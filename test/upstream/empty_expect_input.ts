// Not upstream Tailwind: a hand-written stand-in for the shapes Prettier
// produces in utilities.test.ts, so the .toEqual('') scan is pinned on an
// assertion that does not fit on one line.
test('wrapped-empty', async () => {
  expect(await run(['wrapped-keep'])).toMatchInlineSnapshot(`
    "
    .wrapped-keep {
      display: block;
    }
    "
  `)
  expect(await run(['-wrapped-keep', 'wrapped-keep/foo'])).toEqual(
    '',
  )
})

test('inline-empty', async () => {
  expect(await run(['-inline-empty'])).toEqual('')
})

test('wrapped-non-empty', async () => {
  expect(await run(['wrapped-non-empty'])).toEqual(
    'not empty',
  )
})

test('after-the-wrapping', async () => {
  expect(await run(['-after-the-wrapping'])).toEqual('')
})
