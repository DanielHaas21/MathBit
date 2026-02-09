import {
  Body,
  Controller,
  Post,
  Res,
  Response,
  Route,
  SuccessResponse,
  Tags,
  type TsoaResponse,
} from 'tsoa';
import {
  type MathEngineErrorResponse,
  type MathEngineSolveRequest,
  type MathEngineSolveResponse,
} from '../model/MathEngine';
import { getAppConfig } from '../appConfig';
/**
 * Controller for communicating with the Math solving HTTP microservice .
 * Provides a single endpoint for solving math expressions.
 *
 * Solving is an ambiguous term here, it can mean simplification, solving equations, derivation, integration, etc.
 */
@Route('math_problems')
@Tags('MathProblems')
export class MathEngineController extends Controller {
  constructor() {
    super();
  }

  /**
   *
   * This endpoint accepts a raw math expresion and returns the solved expression from the Haskell microservice. all in LaTeX format
   *
   * **This is also the only endpoint that uses a raw HTTP fetch**
   *
   * @param body Solve request; consists of rawExpression
   * @returns {MathEngineSolveResponse} Solved math expression
   */
  @Post('solve')
  @SuccessResponse('200', 'Solved')
  @Response<MathEngineErrorResponse>('400', 'Bad Request')
  async solveMathExpression(
    @Body() body: MathEngineSolveRequest,
    @Res() badRequest: TsoaResponse<400, MathEngineErrorResponse>
  ): Promise<MathEngineSolveResponse> {
    const requestBody = JSON.stringify({ rawExpression: body.rawExpression });

    const config = getAppConfig();

    const res = await fetch(config.haskellEngine.url + '/solve', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: requestBody,
    });

    if (res.status === 400) {
      const data: Partial<MathEngineErrorResponse> | undefined = await res
        .json()
        .catch(() => undefined);

      return badRequest(400, {
        errHTTPCode: data?.errHTTPCode ?? 400,
        errReason: data?.errReason ?? 'Bad Request',
        errMessage: data?.errMessage ?? 'Haskell engine rejected the request',
      });
    }

    if (!res.ok) {
      throw new Error(`Haskell engine responded with status ${res.status}`);
    }

    const response: MathEngineSolveResponse = await res.json();

    return response;
  }
}
