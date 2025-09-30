import { Controller, Get, Post, Body, Route, Tags, SuccessResponse, Path, Security } from 'tsoa';
import bcrypt from 'bcrypt';
import { type CreateUser } from '../dto/CreateUser';
import { type User } from '../dto/User';
import { UserRepository, User as DBuser } from 'db';
import { UserMapper } from '../mappers/UserMapper';
/**
 * Controller for managing users.
 * Provides endpoints for creating and retrieving users.
 */
@Route('users')
@Tags('Users')
export class UserController extends Controller {
  private userRepository: UserRepository;

  constructor() {
    super();
    this.userRepository = new UserRepository();
  }
  /**
   * Creates a new user.
   * This endpoint accepts user data and creates a new user in the system.
   *
   * @param user The user data to create
   * @returns {User} The created user data
   */
  @SuccessResponse('201', 'Created')
  @Post()
  public async createUser(@Body() user: CreateUser): Promise<User | null> {
    // hash password before saving
    const hashedPassword = await bcrypt.hash(user.password, 10);
    const dbUser: DBuser = { ...user, password: hashedPassword } as DBuser;

    const id = await this.userRepository.createUser(dbUser);
    if (id) {
      const newUser = await this.userRepository.getUserById(id);
      this.setStatus(201);
      return UserMapper.toCreateModel(newUser!); // can never be null
    }

    throw new Error('User not created');
  }
  /**
   * Creates a new user.
   * This endpoint accepts user data and creates a new user in the system.
   *
   * @param user The user data to create
   * @returns {User} The created user data
   */
  @Get()
  @Security('jwt')
  public async getAllUsers(): Promise<{ users: User[] }> {
    const users = await this.userRepository.getAllUsers();
    return { users: users.map((u) => UserMapper.toDto(u)) as User[] };
  }
  /**
   * Retrieves a user by their ID.
   *
   * @param id The ID of the user to retrieve
   * @returns {User} The user data
   */
  @Get('{id}')
  @Security('jwt')
  public async getUserById(@Path() id: number): Promise<User | null> {
    const user = await this.userRepository.getUserById(id);
    if (user) {
      return UserMapper.toDto(user);
    }
    throw new Error('Error mapping user');
  }
}
