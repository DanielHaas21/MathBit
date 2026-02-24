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
  Put,
  Delete,
  Query,
} from '@tsoa/runtime';
import { MathProblemStepRepository } from 'db';
import { type MathProblemStep } from '../dto/MathProblemStep';
import { MathProblemStepMapper } from '../mappers/MathProblemStepMapper';
import {
  type UpdateMathProblemStepRequest,
  type CreateMathProblemStepRequest,
} from '../dto/Request/MathProblemStepRequest';

/**
 * Controller for managing Math problem steps.
 * Provides endpoints for creating, updating, deleting and retrieving math problem steps.
 */
@Route('math_problem_steps')
@Tags('MathProblemSteps')
export class MathProblemStepController extends Controller {
  private MathProblemStepRepository: MathProblemStepRepository;

  constructor() {
    super();
    this.MathProblemStepRepository = new MathProblemStepRepository();
  }

  /**
   * Creates a Math problem step.
   * This endpoint accepts math problem step data and creates a new math problem step in the system.
   *
   * @param body Create request; consists of problemId, and step data
   * @returns {MathProblemStep} The created math problem step data
   */
  @SuccessResponse('201', 'Created')
  @Post()
  @Security('jwt')
  public async createMathProblemStep(
    @Body() body: CreateMathProblemStepRequest
  ): Promise<MathProblemStep | null> {
    const id = await this.MathProblemStepRepository.createMathProblemStep(
      body.problemId,
      body.step
    );
    if (id) {
      const newMathProblemStep = await this.MathProblemStepRepository.getMathProblemStepById(id);
      this.setStatus(201);

      if (!newMathProblemStep) {
        throw new Error('Math problem step not found');
      }
      return MathProblemStepMapper.toDto(newMathProblemStep);
    }

    throw new Error('Math problem step not created');
  }

  /**
   * Retrieves a Math problem step.
   * This endpoint accepts an id to retrieve a math problem step in the system
   *
   * @param  problemId id of the problem
   * @returns {MathProblemStep} The retrieved math problem data
   */
  @Get('by-problem/{problemId}')
  @Security('jwt')
  public async getMathProblemStepsByProblemId(
    @Path() problemId: number
  ): Promise<{ data: MathProblemStep[] }> {
    const MathProblemStep =
      await this.MathProblemStepRepository.getMathProblemStepsByProblemId(problemId);

    return {
      data: MathProblemStep.map((u) => MathProblemStepMapper.toDto(u)) as MathProblemStep[],
    };
  }

  /**
   * Retrieves a Math problem step.
   * This endpoint accepts an id to retrieve a math problem step in the system
   *
   * @param id
   * @param idType type of the id
   * @param problemId in case type is index this is requiered
   * @returns {MathProblemStep} The retrieved math problem step data
   */
  @Get('{id}')
  @Security('jwt')
  public async getMathProblemStepById(
    @Path() id: number,
    @Query() idType: 'index' | 'id',
    @Query() problemId?: number
  ): Promise<MathProblemStep | null> {
    const MathProblemStep =
      idType === 'id'
        ? await this.MathProblemStepRepository.getMathProblemStepById(id)
        : await this.MathProblemStepRepository.getMathProblemStepByStepIndex(id, problemId ?? 1);

    if (!MathProblemStep) {
      throw new Error('Math problem step not found');
    }
    return MathProblemStepMapper.toDto(MathProblemStep);
  }

  /**
   * Updates a Math problem step.
   * This endpoint accepts a body to update a math problem step in the system
   *
   * @param body Update request; consists of id, and step data
   * @returns {void} no return value
   */
  @SuccessResponse('201', 'Updated')
  @Put()
  @Security('jwt')
  public async updateMathProblemStep(@Body() body: UpdateMathProblemStepRequest): Promise<void> {
    await this.MathProblemStepRepository.updateMathProblemStep(body.id, body.step);
  }

  /**
   * Deletes a Math problem steps.
   * This endpoint accepts an id to delete a math problem step in the system
   *
   * @param problemId id of the problem
   * @returns {void} no return value
   */
  @Delete('{id}')
  @Security('jwt')
  public async deleteMathProblemStep(@Path() id: number): Promise<void> {
    await this.MathProblemStepRepository.deleteMathProblemStepByProblemId(id);
  }

  /**
   * Deletes Math problem steps by problem id.
   *
   * This endpoint accepts a problem id to delete all math problem steps related to that problem in the system
   *  *
   * @param problemId id of the problem
   * @returns {void} no return value
   * */
  @Delete('problemId/{problemId}')
  @Security('jwt')
  public async deleteMathProblemStepsByProblemId(@Path() problemId: number): Promise<void> {
    await this.MathProblemStepRepository.deleteMathProblemStepByProblemId(problemId);
  }
}
