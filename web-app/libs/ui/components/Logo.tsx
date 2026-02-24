import { Label } from './Label';
import logo from '../assets/images/dark/logo_medium.svg';
import { cn } from '../utils';

export interface LogoProps {
  className?: string;
}

export const Logo = ({ className }: LogoProps) => {
  return (
    <div style={{ display: '-webkit-inline-box' }} className={cn('', className)}>
      <Label size="lg" className="text-[46px] font-bold select-none ps-4">
        Mathbit
      </Label>
      <img src={logo} alt="Mathbit Logo" className="ps-2 " />
    </div>
  );
};

Logo.displayName = 'Logo';
