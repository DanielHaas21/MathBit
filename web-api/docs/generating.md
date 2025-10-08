# MathBit - `generating`

This doc file explains the workflow of generating and general funcionality

### Workflow

`/controllers` contains definitions for are all exported routes. TSOA
route specifications are declared via `@` things such as:

- @Route('users')
- @Tags('Users')
- @SuccessResponse('201', 'Created')
- @Post()
- @Security('jwt')

The **generate** command then generates a unused TSOA SDK, and a Swagger SDK which contains an open api specification, which is then written into [schema.json](../../web-api-client/schema/schema.json)

From which kubb generates api-client routes