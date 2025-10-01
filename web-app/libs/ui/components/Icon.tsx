import { FontAwesomeIcon, FontAwesomeIconProps } from '@fortawesome/react-fontawesome';
import { cva, type VariantProps } from 'class-variance-authority';
import { IconDefinition, library } from '@fortawesome/fontawesome-svg-core';

import { cn } from '../utils';
import { IconName } from '../icons/names';

import icons from '../icons/icons.json';
library.add(icons as IconDefinition[]);

const iconVariants = cva(
  `
  `,
  {
    variants: {
      size: {
        xs: 'text-xs',
        sm: 'text-sm',
        md: 'text-md',
        lg: 'text-lg',
        xl: 'text-xl',
        '2xl': 'text-2xl'
      },
      rotate: {
        0: '',
        90: 'fa-rotate-90',
        180: 'fa-rotate-180',
        270: 'fa-rotate-270'
      }
    },
    defaultVariants: {
      size: 'md'
    }
  }
);

type IconVariants = VariantProps<typeof iconVariants>;

export interface IconProps
  extends Omit<FontAwesomeIconProps, 'rotate' | 'size' | 'icon'>,
    IconVariants {
  name: IconName;
  className?: string;
}

export const Icon = (props: IconProps) => {
  const { name, className, size, rotate, ...other } = props;
  return (
    <FontAwesomeIcon
      icon={['far', name]}
      className={cn(iconVariants({ className, size, rotate }))}
      {...other}
    />
  );
};
