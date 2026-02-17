import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';

const AvatarVariants = cva(`rounded-full border border-white-800`, {
  variants: {
    variant: {
      default: 'flex items-center justify-center bg-primary-base text-text-white font-semibold',
      inverted: 'flex items-center justify-center bg-white-50 text-primary-base font-semibold',
    },
    size: {
      sm: 'w-[30px] h-[30px] text-sm',
      md: 'w-[40px] h-[40px] text-md',
      lg: 'w-[50px] h-[50px] text-xl',
    },
  },
  defaultVariants: {
    variant: 'default',
    size: 'md',
  },
});

type AvatarVariantprops = VariantProps<typeof AvatarVariants>;

export interface AvatarProps extends AvatarVariantprops {
  src?: string | undefined;
  name?: string | undefined;
}

export const Avatar: React.FC<AvatarProps> = ({ src, name, variant, size }) => {
  return src ? (
    <img src={src} className={cn(AvatarVariants({ size }))}></img>
  ) : name ? (
    <div className={cn(AvatarVariants({ variant, size }))}>
      {name?.charAt(0)}
      {name?.charAt(1)}
    </div>
  ) : (
    <div className={cn(AvatarVariants({ variant, size }))}>?</div>
  );
};
Avatar.displayName = 'Avatar';
