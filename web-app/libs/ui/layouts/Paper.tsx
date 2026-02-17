import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import * as React from 'react';
import { Divider, DividerProps } from '../components';

const paperVariants = cva(
  `
    bg-background-text
    rounded-lg
    p-5
  `
);

type PaperVariants = VariantProps<typeof paperVariants>;

export interface PaperProps extends PaperVariants, DividerProps {
  className?: string;
  children?: React.ReactNode;
  showDivider?: boolean;
}

export interface PaperTitleProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PaperActionProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PaperContentProps {
  className?: string;
  children?: React.ReactNode;
}

export function Paper(props: PaperProps) {
  const {
    className,
    children,
    showDivider = true,
    size = 'full',
    thickness = 'sm',
    variant = 'light',
  } = props;

  const childrenArray = React.Children.toArray(children);

  const titleOrActionChildren = childrenArray.filter(
    (child: any) =>
      child?.type?.displayName === 'Paper.Title' || child?.type?.displayName === 'Paper.Action'
  );

  const otherChildren = childrenArray.filter(
    (child: any) =>
      child?.type?.displayName !== 'Paper.Title' && child?.type?.displayName !== 'Paper.Action'
  );

  const hasTitleOrAction = titleOrActionChildren.length > 0;

  return (
    <section
      className={cn(
        'grid gap-2 grid-cols-[1fr_auto] content-start',
        "[grid-template-areas:'title_action'_'divider_divider'_'content_content']",
        paperVariants({ className })
      )}
    >
      {titleOrActionChildren.map((child, index) => (
        <React.Fragment key={`ta-${index}`}>{child}</React.Fragment>
      ))}

      {showDivider && hasTitleOrAction && (
        <div className="[grid-area:divider] ">
          <Divider size={size} variant={variant} thickness={thickness} className="mt-[5px]" />
        </div>
      )}

      {otherChildren.map((child, index) => (
        <React.Fragment key={`other-${index}`}>{child}</React.Fragment>
      ))}
    </section>
  );
}

Paper.Title = ((props: PaperTitleProps) => {
  const { children, className } = props;
  return <div className={cn('py-2 text-xl [grid-area:title]', className)}>{children}</div>;
}) as React.FC<PaperTitleProps>;
Paper.Title.displayName = 'Paper.Title';

Paper.Action = ((props: PaperActionProps) => {
  const { children, className } = props;
  return <div className={cn('py-2 [grid-area:action]', className)}>{children}</div>;
}) as React.FC<PaperActionProps>;
Paper.Action.displayName = 'Paper.Action';

Paper.Content = ((props: PaperContentProps) => {
  const { children, className } = props;
  return <div className={cn('[grid-area:content]', className)}>{children}</div>;
}) as React.FC<PaperContentProps>;
Paper.Content.displayName = 'Paper.Content';
