import { FontAwesomeIcon, FontAwesomeIconProps } from '@fortawesome/react-fontawesome';
import { cva, type VariantProps } from 'class-variance-authority';
import { IconName, RotateProp } from '@fortawesome/fontawesome-svg-core';
import { cn } from '../utils';

const iconVariants = cva('', {
  variants: {
    size: {
      xs: 'text-xs',
      sm: 'text-sm',
      md: 'text-base',
      lg: 'text-lg',
      xl: 'text-xl',
      '2xl': 'text-2xl',
    },
  },
  defaultVariants: {
    size: 'md',
  },
});

type IconVariants = VariantProps<typeof iconVariants>;

export interface IconProps extends Omit<FontAwesomeIconProps, 'icon' | 'size'>, IconVariants {
  name: IconName;
  type?: 'fas' | 'far' | 'fab';
  rotate?: RotateProp | undefined;
  className?: string;
}

export const Icon = ({ name, type = 'far', size, rotate, className, ...other }: IconProps) => {
  return (
    <FontAwesomeIcon
      icon={[type, name]}
      rotation={rotate}
      className={cn(iconVariants({ size }), className)}
      {...other}
    />
  );
};
