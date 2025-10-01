'use client';
import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { Dialog, DialogPanel } from '@headlessui/react';
import { Icon } from './Icon';
import { motion, AnimatePresence } from 'framer-motion';
import { Label } from './Label';

interface LayoutProp {
  layout?: 'row' | 'vertical';
}

const ModalVariants = cva(``, {
  variants: {
    size: {
      xs: 'w-[400px]',
      sm: 'w-[600px]',
      md: 'w-[700px]',
      lg: 'w-[800px]',
      xl: 'w-[900px]',
      '2xl': 'w-[1000px]',
      half: 'w-[50vw]',
      full: 'w-full'
    }
  }
});

type ModalVariantProps = VariantProps<typeof ModalVariants>;

export interface ModalProps extends ModalVariantProps, LayoutProp {
  open: boolean;
  onClose: () => void;
  children: React.ReactNode;
  className?: string;
}

const ModalBase = React.forwardRef<HTMLElement, ModalProps>(
  ({ children, open, onClose, size, className, layout = 'row' }, ref) => {
    return (
      <AnimatePresence>
        {open && (
          <Dialog
            ref={ref}
            open={open}
            onClose={onClose}
            as="div"
            className="relative z-[100] w-full"
          >
            <motion.div
              key="backdrop"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              transition={{ duration: 0.25 }}
              className="fixed inset-0 bg-overlays-50 p-4"
            />

            <div className="fixed inset-0 flex w-screen items-center justify-center p-4">
              <motion.div
                key="modal"
                initial={{ opacity: 0, scale: 0.95 }}
                animate={{ opacity: 1, scale: 1 }}
                exit={{ opacity: 0, scale: 0.95 }}
                transition={{ duration: 0.3, ease: 'easeOut' }}
              >
                <DialogPanel
                  className={cn(
                    'rounded-xl bg-white-50 overflow-hidden shadow-xl',
                    ModalVariants({ size }),
                    className,
                    layout === 'row' ? 'flex flex-row flex-wrap' : 'flex flex-col'
                  )}
                >
                  {React.Children.map(children, (child) => {
                    if (!React.isValidElement(child)) return child;
                    return React.cloneElement(child as React.ReactElement<LayoutProp>, {
                      layout
                    });
                  })}
                </DialogPanel>
              </motion.div>
            </div>
          </Dialog>
        )}
      </AnimatePresence>
    );
  }
);

ModalBase.displayName = 'ModalBase';

const ModalHeaderVariants = cva(``, {
  variants: {
    variant: {
      white: 'bg-white-50',
      grey: 'bg-white-100'
    }
  },
  defaultVariants: {
    variant: 'grey'
  }
});

type ModalHeaderVariantProps = VariantProps<typeof ModalHeaderVariants>;

export interface ModalHeader extends LayoutProp, ModalHeaderVariantProps {
  className?: string;
  onClose: () => void;
  children?: React.ReactNode;
  label?: string;
  description?: string;
}

const Header: React.FC<ModalHeader> = ({
  className,
  onClose,
  children,
  label,
  description,
  layout = 'row',
  variant
}) => {
  return (
    <div
      className={cn(
        className,
        'w-[100%] min-h-[120px] flex flex-row flex-nowrap items-center justify-between ',
        layout === 'row' && ' border-b border-b-white-800 ',
        ModalHeaderVariants({ variant })
      )}
    >
      <div className=" p-5 min-h-[120px] flex flex-col items-start whitespace-normal">
        <Label size="xl" className="font-medium">
          {label}
        </Label>
        <p className="text-sm font-light text-text-grey w-[200px]">{description}</p>
        {children}
      </div>
      <div className="p-5">
        <Icon
          name="xmark"
          onClick={onClose}
          className="cursor-pointer text-xl text-text-grey transition hover:text-text-black"
        ></Icon>
      </div>
    </div>
  );
};

const ModalContentVariants = cva(``, {
  variants: {
    size: {
      sm: 'h-[400px]',
      md: 'h-[500px]',
      lg: 'h-[600px]',
      xl: 'h-[800px]',
      half: 'h-[50vh]',
      full: 'h-[85vh]'
    }
  },
  defaultVariants: {
    size: 'md'
  }
});

type ModalContentVariantProps = VariantProps<typeof ModalContentVariants>;

export interface ModalContent extends ModalContentVariantProps, LayoutProp {
  className?: string;
  children?: React.ReactNode;
}

const Content: React.FC<ModalContent> = ({ className, children, size, layout = 'row' }) => {
  return (
    <div
      className={cn(
        className,
        'overflow-y-auto p-4 pt-6 pb-6 flex flex-col gap-2',
        layout === 'row' ? 'w-[85%]' : 'border-b border-b-white-800 border-t border-t-white-800',
        ModalContentVariants({ size })
      )}
    >
      {children}
    </div>
  );
};

const ModalFooterVariants = cva(``, {
  variants: {
    variant: {
      white: 'bg-white-50',
      grey: 'bg-white-100'
    }
  },
  defaultVariants: {
    variant: 'grey'
  }
});

type ModalFooterVariantProps = VariantProps<typeof ModalFooterVariants>;

export interface ModalFooter extends LayoutProp, ModalFooterVariantProps {
  className?: string;
  children?: React.ReactNode;
}

const Footer: React.FC<ModalFooter> = ({ className, children, layout = 'row', variant }) => {
  return (
    <div
      className={cn(
        className,
        'min-h-[80px] flex flex-row flex-nowrap items-center justify-between p-5',
        layout === 'row' && 'border-s border-s-white-800 w-[15%] flex-col justify-start',
        ModalFooterVariants({ variant })
      )}
    >
      {children}
    </div>
  );
};

export interface ModalAction extends LayoutProp {
  className?: string;
  children?: React.ReactNode;
}

const Action: React.FC<ModalAction> = ({ className, children, layout = 'row' }) => {
  return (
    <div className={cn(className, 'flex gap-2', layout === 'row' ? 'flex-col' : 'flex-row')}>
      {children}
    </div>
  );
};

const Modal = Object.assign(ModalBase, {
  Header: Header,
  Content: Content,
  Footer: Footer,
  Action: Action
});

export { Modal };

Modal.displayName = 'Modal';
Modal.Header.displayName = 'ModalHeader';
Modal.Content.displayName = 'ModalContent';
Modal.Footer.displayName = 'ModalFooter';
Modal.Action.displayName = 'ModalAction';
