/**
 * Query for a math problem search
 */
export interface MathProblemQuery {
  userId?: number;
  name?: string;
  bookmark?: boolean;
  dateFrom?: Date;
  dateTo?: Date;
}
