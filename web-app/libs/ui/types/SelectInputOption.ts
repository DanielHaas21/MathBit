import { IconName } from '../icons/names';

/**
 *  Represents a single option for a SelectInput element
 */
export type SelectInputOption = {
  value: any;
  label: string;
  icon?: IconName;
  disabled?: boolean;
};
