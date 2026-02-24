import {
  Controller,
  Get,
  Post,
  Body,
  Route,
  Tags,
  SuccessResponse,
  Path,
  Security,
  Query,
  Put,
  Delete,
} from '@tsoa/runtime';
import { MathProblemRepository } from 'db';
import { MathProblemMapper } from '../mappers/MathProblemMapper';
import { MathProblem } from '../dto/MathProblem';
import { type MathProblemQuery } from '../types/problemQuery';
import {
  type CreateMathProblemRequest,
  type UpdateMathProblemRequest,
} from '../dto/Request/MathProblemRequest';

/**
 * Controller for managing Math problems.
 * Provides endpoints for creating, updating, deleting and retrieving math problems.
 */
@Route('math_problems')
@Tags('MathProblems')
export class MathProblemController extends Controller {
  private MathProblemRepository: MathProblemRepository;

  constructor() {
    super();
    this.MathProblemRepository = new MathProblemRepository();
  }

  /**
   * Creates a Math problem.
   * This endpoint accepts math problem data and creates a new math problem in the system.
   *
   * @param body Update request; consists of userId, and problem data
   * @returns {MathProblem} The created math problem data
   */
  @SuccessResponse('201', 'Created')
  @Post()
  @Security('jwt')
  public async createMathProblem(
    @Body() body: CreateMathProblemRequest
  ): Promise<Partial<MathProblem> | null> {
    const id = await this.MathProblemRepository.createMathProblem(body.userId, body.problem);
    if (id) {
      const newMathProblem = await this.MathProblemRepository.getMathProblemById(id);
      this.setStatus(201);

      if (!newMathProblem) {
        throw new Error('Math problem not found');
      }
      return MathProblemMapper.toDto(newMathProblem);
    }

    throw new Error('Math problem not created');
  }

  /**
   * Retrieves a Math problem.
   * This endpoint accepts an id to retrieve a math problem in the system
   *
   * @param id id of the user
   * @returns {MathProblem} The retrieved math problem data
   */
  @Get('{id}')
  @Security('jwt')
  public async getMathProblemById(@Path() id: number): Promise<MathProblem | null> {
    const MathProblem = await this.MathProblemRepository.getMathProblemById(id);

    if (!MathProblem) {
      throw new Error('Math problem not found');
    }
    return MathProblemMapper.toDto(MathProblem);
  }

  /**
   * Retrieves Math problems.
   * This endpoint accepts a query to retrieve a math problems in the system
   * @param query Math problem query
   * @param offset offset of each request
   * @param limit fixed limit for each request
   */
  @Post('search')
  @Security('jwt')
  public async getMathProblems(
    @Body() query: MathProblemQuery,
    @Query() offset: number,
    @Query() limit: number
  ): Promise<{ data: MathProblem[]; offset: number }> {
    const MathProblems = await this.MathProblemRepository.getMathProblems(query, offset, limit);

    return {
      data: MathProblems.map((u) => MathProblemMapper.toDto(u)) as MathProblem[],
      offset: offset,
    };
  }

  /**
   * Updates a Math problem.
   * This endpoint accepts a body to update a math problem in the system
   * @param body Update request; consists of id, and problem data
   * @returns {void} no return value
   */
  @SuccessResponse('201', 'Updated')
  @Put()
  @Security('jwt')
  public async updateMathProblem(@Body() body: UpdateMathProblemRequest): Promise<void> {
    await this.MathProblemRepository.updateMathProblem(body.id, body.problem);
  }

  /**
   * Deletes a Math problem.
   * This endpoint accepts an id to delete a math problem in the system
   *
   * @param id id of the problem
   * @returns {void} no return value
   */
  @Delete('{id}')
  @Security('jwt')
  public async deleteMathProblem(@Path() id: number): Promise<void> {
    await this.MathProblemRepository.deleteMathProblemById(id);
  }
}
