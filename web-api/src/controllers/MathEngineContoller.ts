import {
  Controller,
  Post,
  Body,
  Route,
  Tags,
  Security,
} from 'tsoa';
import { type MathEngineSolveRequest, type MathEngineSolveResponse } from '../model/MathEngine';
import { getAppConfig } from '@/appConfig';

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
  @Security('jwt')
  async solveMathExpression(
    @Body() body: MathEngineSolveRequest
  ): Promise<MathEngineSolveResponse> {
    if (body.rawExpression.trim() === '') {
      throw new Error('rawExpression cannot be empty');
    }

    const config = getAppConfig();

    const response: MathEngineSolveResponse = await fetch(config.haskellEngine.url + '/solve', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(body),
    }).then((res) => {
      if (!res.ok) {
        throw new Error(`Haskell engine responded with status ${res.status}`);
      }
      return res.json();
    });

    return response;
  }
}
