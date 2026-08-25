// Not upstream Tailwind: a hand-written stand-in for the block shapes
// utilities.test.ts uses, so the test( scan is pinned on blocks that sit
// inside one or more describe( blocks.
test('top level', async () => {
  expect(await run(['top-level'])).toMatchInlineSnapshot(`
    "
    .top-level {
      display: block;
    }
    "
  `)
})

describe('outer', () => {
  test('indented once', async () => {
    expect(await run(['indented-once'])).toMatchInlineSnapshot(`
      "
      .indented-once {
        display: flex;
      }
      "
    `)
  })

  test("double quoted name (with a paren in it)", async () => {
    expect(await run(['double-quoted'])).toEqual('')
  })

  test(`backtick name`, async () => {
    expect(await run(['backtick'])).toEqual('')
  })

  test.each([
    ['a', true],
    ['b', false],
  ])('name %s is valid (%s)', (name, valid) => {
    expect(isValidName(name)).toBe(valid)
  })

  test('defines its own utility', async () => {
    expect(
      await run(
        ['custom-thing'],
        css`
          @utility custom-thing {
            display: grid;
          }
          @tailwind utilities;
        `,
      ),
    ).toMatchInlineSnapshot(`
      "
      .custom-thing {
        display: grid;
      }
      "
    `)
  })

  describe('inner', () => {
    test('indented twice', async () => {
      expect(await run(['indented-twice'])).toMatchInlineSnapshot(`
        "
        .indented-twice {
          display: grid;
        }
        "
      `)
    })
  })

  test('after the inner block', async () => {
    expect(await run(['after-inner'])).toEqual('')
  })
})

test('after the outer block', async () => {
  expect(await run(['after-outer'])).toEqual('')
})
